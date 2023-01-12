{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}

{-|
Module      : Handlers
Description : Handlers for PAB connection.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define in this module the handlers for the PAB connection.
-}

module Handlers
    ( -- * Handlers
        Escrow(..)
    )
where

-- Non-IOG imports
import Data.Aeson          ( FromJSON, ToJSON )
import Data.OpenApi.Schema ( ToSchema )
import GHC.Generics        ( Generic )
import Prettyprinter       ( Pretty, pretty, viaShow )

-- IOG imports
import Plutus.PAB.Effects.Contract.Builtin ( HasDefinitions ( getDefinitions
                                                            , getSchema
                                                            , getContract
                                                            )
                                           , SomeBuiltin ( SomeBuiltin )
                                           )

-- Escrow imports
import Escrow ( endpoints )
import Utils.WalletAddress ( WalletAddress )

{- | The `Escrow` type represents the kind of activation of a wallet to the PAB.
    We can only `Connect` providing the Wallet Address of the user to interact
    with the off-chain code endpoints.
-}
newtype Escrow = Connect WalletAddress
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

{- | The `getEscrowContract` function defines which is the definition of each
     kind of activation. `Connect` to the PAB is simply calling the `endpoints`
     function, that will forever wait operation calls.
-}
getEscrowContract :: Escrow -> SomeBuiltin
getEscrowContract (Connect e) = SomeBuiltin $ endpoints e

instance HasDefinitions Escrow where
    getDefinitions = []
    getSchema      = const []
    getContract    = getEscrowContract

instance Pretty Escrow where
    pretty = viaShow
