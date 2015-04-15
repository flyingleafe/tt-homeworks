{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Main where

import Prelude.Unicode
import qualified Data.ByteString.Char8 as BS
import Parser
import ParserUtils
import Substitutions

main ∷ IO ()
main = readEx expr >>= BS.putStrLn ∘ BS.unlines ∘ free
