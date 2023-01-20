{-|
Module      : Tests.Prop.Gen
Description : Arbitrary generation of asset classes and tokens.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

-}

module Tests.Prop.Gen where

-- Non-IOG imports
import Data.Maybe           ( fromJust )
import Data.Text            ( pack )
import Data.ByteString.UTF8 ( fromString )
import Text.Hex             ( decodeHex )
import Test.QuickCheck      ( Gen, elements, chooseInteger, suchThat )

-- IOG imports
import Plutus.Contract.Test   ( Wallet )
import Plutus.V1.Ledger.Value ( Value, CurrencySymbol, TokenName
                              , assetClassValue, assetClass
                              , tokenName, currencySymbol
                              )
import Ledger                 ( AssetClass )
import Ledger.Ada             ( lovelaceValueOf )

-- Escrow imports
import Tests.Utils ( wallets )

{- | Wallet with its value definition. Each one will have 1_000_000 tokens using
     the `allAssetClasses`.
-}
walletsWithValue :: [(Wallet,Value)]
walletsWithValue =
    [ (w, v <>  mconcat (map (`assetClassValue` 1_000_000) allAssetClasses))
    | w <- wallets
    ]
  where
    v :: Value
    v = lovelaceValueOf 100_000_000

-- | List of AssetClass for emulator configuration
allAssetClasses :: [AssetClass]
allAssetClasses =
    [ assetClass (mkCurrencySymbol $ replicate 4 hex)
                 (mkTokenName $ replicate n hex)
    | hex <- ['a'..'f']
    , n <- [1..4]
    ]

-- | Using `wallets` picks one wallet.
genWallet :: Gen Wallet
genWallet = elements wallets

-- | Generates an random integer between 1 and 50.
genPayment :: Gen Integer
genPayment = chooseInteger (1, 50)

-- | Using `allAssetClasses` picks one asset class.
genAssetClass :: Gen AssetClass
genAssetClass = elements allAssetClasses

-- | Generates two different tokens.
gen2Tokens :: Gen ((AssetClass, Integer), (AssetClass,Integer))
gen2Tokens = do
    ac1 <- genAssetClass
    ac2 <- genAssetClass `suchThat` (/= ac1)
    p1 <- genPayment
    p2 <- genPayment
    return ((ac1, p1), (ac2, p2))

-- | Tokenname smart constructor from a String
mkTokenName :: String -> TokenName
mkTokenName = tokenName . fromString

{- | CurrencySymbol smart constructor. Suffix 4 hexa chars to
     "246ea4f1fd944bc8b0957050a31ab0487016be233725c9f931b1".
-}
mkCurrencySymbol :: String -> CurrencySymbol
mkCurrencySymbol s@[_,_,_,_] =
    currencySymbol $ fromJust $ decodeHex $ pack $
    "246ea4f1fd944bc8b0957050a31ab0487016be233725c9f931b1" ++ s
mkCurrencySymbol _ = error "mkCurrencySymbol: Provided less or more than 4 chars"
