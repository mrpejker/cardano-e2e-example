{-# LANGUAGE TypeApplications #-}

module Main (main) where

-- IOG imports
import Plutus.PAB.Effects.Contract.Builtin ( handleBuiltin )
import Plutus.PAB.Run                      ( runWith )

-- Escrow imports
import EscrowHandlers ( Escrow )


main :: IO ()
main = runWith $ handleBuiltin @Escrow