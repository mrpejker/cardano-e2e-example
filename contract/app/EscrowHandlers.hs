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

{- | The `Escrow` type represents the kind of activation of a wallet to the PAB.
    We can only `Connect` providing the Address of the user connecting to the
    contract.
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

{- | The `getEscrowContract` function defines which is the definition of each
     kind of activation. `Connect` to the PAB is simply calling the `endpoints`
     function, that waits for an operation call.
-}
getEscrowContract :: Escrow -> SomeBuiltin
getEscrowContract (Connect a) = SomeBuiltin $ endpoints a
