{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Main where

import System.IO
import qualified Data.ByteString.Char8 as BS
import Prelude.Unicode
import Data.Monoid.Unicode
import LambdaType
import Parser
import ParserUtils
import Utils

showVerbose ∷ Λ → BS.ByteString
showVerbose (Var v) = v
showVerbose (Λ v e) = "(\\" ⊕ v ⊕ "." ⊕ showVerbose e ⊕ ")"
showVerbose (e :@ e') = "(" ⊕ showVerbose e ⊕
                         " " ⊕ showVerbose e' ⊕ ")"
main :: IO ()
main = fileIO "task1.in" "task1.out" $ \inp outp →
  hReadEx inp expr >>= BS.hPutStrLn outp ∘ showVerbose
