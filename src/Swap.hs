{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Swap
    ( bytes
    , writeScriptToFile
    , Offer (..)
    , Action (..)
    ) where

import Plutus.V2.Ledger.Api (adaSymbol, adaToken, mkValidatorScript, unValidatorScript, Script, Validator, TxInfo (..), ScriptContext (..), txInInfoResolved, TxOut(txOutDatum), OutputDatum (NoOutputDatum), toData)

import Plutus.V2.Ledger.Contexts (valuePaidTo, txSignedBy)

import Plutus.Script.Utils.V2.Scripts (
    mkUntypedValidator,
    mkUntypedMintingPolicy,
    scriptCurrencySymbol)

import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (geq)
import Ledger ( POSIXTime, PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash))

-- package plutus-tx --
import PlutusTx.Prelude (traceIfFalse, BuiltinData, Integer, Eq(..), Maybe, Bool(..), (&&), (>=), (<>))

-- Plutus Tx --
import qualified PlutusTx

-- package: plutus-ledger-api --
import Ledger.Typed.Scripts ( ValidatorTypes (..), TypedValidator, mkTypedValidator, validatorScript)
-- Haskell --
import Prelude (Show (..), (.))
import qualified Prelude as Haskell

import Data.ByteString.Short (ShortByteString, toShort)
import Data.ByteString.Lazy (toStrict, ByteString)

import Codec.Serialise (serialise)

-- JSON --
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode)

-- cardano-api
import Cardano.Api (PlutusScriptV2, writeFileTextEnvelope, displayError, scriptDataToJson, ScriptDataJsonSchema (ScriptDataJsonDetailedSchema))
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), fromPlutusData)


data Action
    = Buy   -- buyer purchases the offer --
    | Cancel -- seller withdraws the offer --
    deriving (Show)
PlutusTx.makeIsDataIndexed ''Action [('Buy, 0), ('Cancel, 1)]

data Offer = Offer 
    { price           :: !Integer
    , seller          :: !PaymentPubKeyHash
    } deriving (Show, Generic, ToJSON, FromJSON)
PlutusTx.makeIsDataIndexed ''Offer [('Offer, 0)]

data Swapping
instance ValidatorTypes Swapping where
    type instance DatumType Swapping = ()
    type instance RedeemerType Swapping = ()

{-# INLINABLE script' #-}
script' :: Offer -> Action -> ScriptContext -> Bool
script' Offer{price, seller} act ScriptContext{scriptContextTxInfo} = 
    case act of 
        Buy -> sellerPaid' && input
        Cancel -> sellerSigned'
    where
        input =
            let 
                isScriptInput i = case (txOutDatum . txInInfoResolved) i of
                    NoOutputDatum -> Haskell.False
                    _  -> Haskell.True
                xs = [i | i <- txInfoInputs scriptContextTxInfo, isScriptInput i]
            in
                case xs of
                    [i] -> Haskell.True
                    _   -> traceIfFalse "expected 1 script input" Haskell.False

        sellerPaid' = traceIfFalse "seller not paid" sellerPaid
        sellerPaid =
            let val = valuePaidTo scriptContextTxInfo (unPaymentPubKeyHash seller)
            in val `geq` lovelaceValueOf price

        sellerSigned' = traceIfFalse "only seller may cancel" sellerSigned
        sellerSigned = txSignedBy scriptContextTxInfo (unPaymentPubKeyHash seller)


validator :: Validator
validator = mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = mkUntypedValidator script'

script :: Script
script = unValidatorScript validator

bytes :: ShortByteString
bytes = (toShort . toStrict) (serialise script)

plutusScript :: PlutusScript PlutusScriptV2
plutusScript = PlutusScriptSerialised bytes

writeScriptToFile :: Haskell.IO ()
writeScriptToFile = do
    result <- writeFileTextEnvelope "swap.plutus" Haskell.Nothing plutusScript
    case result of
        Haskell.Left err -> Haskell.print Haskell.$ displayError err
        Haskell.Right () -> Haskell.return ()

-- plutusDataToJSON :: ByteString
-- plutusDataToJSON = encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . toData Haskell.$ (Offer 10 (PaymentPubKeyHash "90ea221c035ac0ebc3038d32f2509c95c06bf667bb3637684dc24174"))