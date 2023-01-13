{-|
Module      : Main
Description : Main function to run the PAB.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Main (main) where

-- IOG imports
import Plutus.PAB.Effects.Contract.Builtin ( handleBuiltin )
import Plutus.PAB.Run                      ( runWith )

-- Escrow imports
import Handlers ( Escrow )

main :: IO ()
main = runWith $ handleBuiltin @Escrow
