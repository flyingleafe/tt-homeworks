{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Main where

import Prelude.Unicode
import qualified Data.ByteString.Char8 as BS
import Utils
import Parser
import ParserUtils
import Substitutions

main ∷ IO ()
main = fileIO "task2.in" "task2.out" $ \inp outp →
      hReadEx inp expr >>= BS.hPutStr outp ∘ BS.unlines ∘ free
