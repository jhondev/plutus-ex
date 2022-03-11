module Main where

import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Scripts as Scripts hiding (validatorHash)
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import PlutusTx (Data (..))
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Prelude (IO, Semigroup (..), String)

-- validator gets 3 pices (datum, redeemer and context (consuming tx))
-- we use unit as return value because in case of an invalid validation the script should panic
{-# INLINABLE mkValidator #-} --allow compiler ...
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||]) --give us the plutus syntax tree

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator