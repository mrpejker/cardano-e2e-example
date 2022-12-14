{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}

{-|
Module      : EscrowHandlers
Description : Handlers for PAB connection.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define in this module the handlers for the PAB connection.
-}

module EscrowHandlers
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
import Ledger ( Address )
import Plutus.PAB.Effects.Contract.Builtin ( HasDefinitions ( getDefinitions
                                                            , getSchema
                                                            , getContract
                                                            )
                                           , SomeBuiltin ( SomeBuiltin )
                                           )

-- Escrow imports
import Escrow ( endpoints )

{- | The `Escrow` type represents a connection to an instance of the PAB.
     It is constructed by providing the Address of the user connecting to
     the contract.
-}
newtype Escrow = Connect Address
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Pretty Escrow where
    pretty = viaShow

instance HasDefinitions Escrow where
    getDefinitions = []
    getSchema = const []
    getContract = getEscrowContract

{- | The `getEscrowContract` function defines how to activate the endpoints.
     for a specific user using their address
-}
getEscrowContract :: Escrow -> SomeBuiltin
getEscrowContract (Connect a) = SomeBuiltin $ endpoints a
