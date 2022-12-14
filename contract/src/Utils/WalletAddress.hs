{-|
Module      : Utils.WalletAddress
Description : Util data type for connecting the wallet address to PAB.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.WalletAddress
    ( -- * WalletAddress Type
      WalletAddress
    , mkWalletAddress
      -- * Translation functions
    , toWalletAddress
    , fromWalletAddress
) where

-- Non-IOG imports
import Data.OpenApi ( ToSchema )
import Data.Aeson   ( FromJSON, ToJSON )
import GHC.Generics ( Generic )

-- IOG imports
import Ledger.Credential ( Credential(..), StakingCredential(..) )
import Ledger            ( Address(..), PubKeyHash )

{- | WalletAddress is the representation of public key addresses in the Cardano
     networks. It consists on a public key hash, along with an optional staking
     key hash.
-}
data WalletAddress = WalletAddress { waPayment :: PubKeyHash
                                   , waStaking :: Maybe PubKeyHash
                                   }
    deriving (Show, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Eq WalletAddress where
    {-# INLINABLE (==) #-}
    w1 == w2 =  waPayment w1 == waPayment w2
             && waStaking w1 == waStaking w2

mkWalletAddress :: PubKeyHash -> Maybe PubKeyHash -> WalletAddress
mkWalletAddress pkh stk = WalletAddress { waPayment = pkh
                                        , waStaking = stk
                                        }

fromWalletAddress :: WalletAddress -> Address
fromWalletAddress walletAddress = Address
  { addressCredential = toCredential $ waPayment walletAddress
  , addressStakingCredential = toStakingCredential <$> waStaking walletAddress
  }

toCredential :: PubKeyHash -> Credential
toCredential = PubKeyCredential

toStakingCredential :: PubKeyHash -> StakingCredential
toStakingCredential pubKeyHash = StakingHash $ toCredential pubKeyHash

toWalletAddress :: Address -> Maybe WalletAddress
toWalletAddress address = case addressCredential address of
  PubKeyCredential pubKeyHash -> Just $ mkWalletAddress
    pubKeyHash
    (addressStakingCredential address >>= toPubKeyHash)
  ScriptCredential _ -> Nothing

toPubKeyHash :: StakingCredential -> Maybe PubKeyHash
toPubKeyHash stakingCredential = case stakingCredential of
  StakingHash (PubKeyCredential pubKeyHash) -> Just pubKeyHash
  StakingHash (ScriptCredential _) -> Nothing
  StakingPtr {} -> Nothing