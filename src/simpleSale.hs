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

{-# INLINABLE saleValidator #-}

saleValidator :: BuiltinData -> BuiltinData -> ScriptContext -> ()
saleValidator _ r _
  | r == I 27 = ()
  | otherwise = traceError "Incorrect Redeemer!"
  
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| saleValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

srcAddress :: Ledger.Address
srcAddress = scriptAddress validator

