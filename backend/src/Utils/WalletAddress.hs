{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Utils.WalletAddress
Description : Utility data type for connecting the wallet address to PAB.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.WalletAddress
    ( -- * WalletAddress Type
      WalletAddress(..)
    -- * Smart Constructors
    , mkWalletAddress
    -- * Translation functions
    , toWalletAddress
    , fromWalletAddress
    -- * Getters
    , waPaymentPubKeyHash
    -- * Constraints
    , mustPayToWalletAddress
) where

-- Non-IOG imports
import Prelude qualified as HP ( Eq, Show, Ord )
import Data.OpenApi ( ToSchema )
import Data.Aeson   ( FromJSON, ToJSON )
import GHC.Generics ( Generic )

-- IOG imports
import PlutusTx          ( makeIsDataIndexed, makeLift )
import PlutusTx.Prelude  ( Maybe(..), Eq(..), Ord(..)
                         , (&&), ($), (<$>), (.)
                         )
import Ledger            ( Value, Address(..), PubKeyHash
                         , PaymentPubKeyHash(..), stakingCredential
                         , toPubKeyHash
                         )
import Ledger.Credential  ( Credential(..), StakingCredential(..) )
import Ledger.Constraints ( TxConstraints, mustPayToAddress
                          )
{- | WalletAddress is the representation of public key addresses in the Cardano
     networks. It consists on a public key hash, along with an optional staking
     key hash.
-}
data WalletAddress = WalletAddress { waPayment :: PubKeyHash
                                   , waStaking :: Maybe PubKeyHash
                                   }
    deriving (HP.Eq, HP.Show, HP.Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Eq WalletAddress where
    {-# INLINABLE (==) #-}
    w1 == w2 =  waPayment w1 == waPayment w2
             && waStaking w1 == waStaking w2

instance Ord WalletAddress where
    {-# INLINABLE (<=) #-}
    w1 <= w2 =  waPayment w1 <= waPayment w2
             && waStaking w1 <= waStaking w2

-- | Smart constructor of a wallet address.
{-# INLINABLE mkWalletAddress #-}
mkWalletAddress :: PubKeyHash -> Maybe PubKeyHash -> WalletAddress
mkWalletAddress pkh stk = WalletAddress { waPayment = pkh
                                        , waStaking = stk
                                        }

-- | Translate a wallet address to the corresponding ledge address.
{-# INLINABLE fromWalletAddress #-}
fromWalletAddress :: WalletAddress -> Address
fromWalletAddress walletAddress =
    Address
    { addressCredential = PubKeyCredential $ waPayment walletAddress
    , addressStakingCredential = toStakingCredential <$> waStaking walletAddress
    }

-- | Translate, if possible, a ledger address to the corresponding wallet address.
{-# INLINABLE toWalletAddress #-}
toWalletAddress :: Address -> Maybe WalletAddress
toWalletAddress address =
    (`mkWalletAddress` stkPubKeyHash) <$> toPubKeyHash address
  where
    stkPubKeyHash :: Maybe PubKeyHash
    stkPubKeyHash = case stakingCredential address of
                        Just (StakingHash (PubKeyCredential pkh)) -> Just pkh
                        _ -> Nothing

-- | Construct a staking credential from a pubkeyhash
{-# INLINABLE toStakingCredential #-}
toStakingCredential :: PubKeyHash -> StakingCredential
toStakingCredential = StakingHash . PubKeyCredential

waPaymentPubKeyHash :: WalletAddress -> PaymentPubKeyHash
waPaymentPubKeyHash = PaymentPubKeyHash . waPayment

-- | Creates an transaction output using `mustPayToAddress`
mustPayToWalletAddress :: forall i o
                       .  WalletAddress
                       -> Value
                       -> TxConstraints i o
mustPayToWalletAddress wa = mustPayToAddress (fromWalletAddress wa)

-- | Boilerplate for deriving the FromData and ToData instances.
makeLift ''WalletAddress
makeIsDataIndexed ''WalletAddress [ ('WalletAddress, 0) ]
