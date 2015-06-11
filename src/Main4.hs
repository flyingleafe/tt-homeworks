{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Main where

import System.IO
import Eval
import Utils
import Parser
import ParserUtils

main ∷ IO ()
main = fileIO "task4.in" "task4.out" $ \inp outp → do
         term ← hReadEx inp expr
         hPutStrLn outp $ show $ normalize term
