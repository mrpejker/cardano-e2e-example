{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}

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

newtype Escrow = Connect Address
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Pretty Escrow where
    pretty = viaShow

instance HasDefinitions Escrow where
    getDefinitions = []
    getSchema = const []
    getContract = getEscrowContract

getEscrowContract :: Escrow -> SomeBuiltin
getEscrowContract = \case
    Connect a -> SomeBuiltin $ endpoints a
