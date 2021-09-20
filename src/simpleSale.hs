{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module simpleSale where

import            Control.Monad       hiding (fmap)
import            Data.map            as Map
import            Data.Text           (Text)
import            Data.Void           (Void)
import            Plutus.Contract
import            PlutusTx            (Data (..))
import  qualified PlutusTx
import            PlutusTx.Prelude    hiding (Semigroup(..), unless)
import            Ledger              hiding (singleton)
import            Ledger.Constraints  as Constraints
import  qualified Ledger.Scripts      as Scripts
import            Ledger.Ada          as Ada
import            Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Create the validator, as inlinable
{-# INLINABLE saleValidator #-}
saleValidator :: () -> Integer -> ScriptContext -> ()
saleValidator _ r _
  -- Redeemer must match the integer 27 or fails
  | r == I 27 = ()
  | otherwise = traceError "Incorrect Redeemer!"

-- Compile the validator to plutus core
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| saleValidator ||])

-- Get the hash of the validator
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

-- Get the address of the validator
srcAddress :: Ledger.Address
srcAddress = scriptAddress validator

