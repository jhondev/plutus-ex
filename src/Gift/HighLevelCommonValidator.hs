{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad hiding (fmap)
import Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import PlutusTx (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Prelude (IO, Semigroup (..), String)

-- validator gets 3 pices (datum, redeemer and context (consuming tx))
{-# INLINEABLE mkValidator #-}
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ redeemer _ = traceIfFalse "wrong redeemer" (redeemer == 42)

data Typed

instance Scripts.ValidatorTypes Typed where
  type DatumTyped Typed = ()
  type RedeemerType Typed = Integer

typeValidator :: Scripts.TypedValidator Typed
typeValidator =
  Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @Integer

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [||mkValidator||]) --give us the plutus syntax tree

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator