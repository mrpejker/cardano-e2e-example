{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Escrow.Business
Description : Business logic for escrow contract (congestive version).
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define in this module any business logic of the contract. This is
the core part in which any modification of the contract state is defined.
This module will be shared between offchain and onchain code, so it must use
the plutus prelude, instead of the haskell prelude.
-}

module Escrow.Business where
