{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Main where

import Utils
import Parser
import ParserUtils
import Inference
import System.IO
import qualified Data.ByteString.Char8 as BS

main ∷ IO ()
main = fileIO "task6.in" "task6.out" $ \inp outp → do
         λ ← hReadEx inp expr
         case infer λ of
           Nothing → hPutStrLn outp "Лямбда-выражение не имеет типа."
           Just (s, t) → do
                        hPutStrLn outp $ showType t
                        hPutStr outp $ unlines $ map showEq s
                            where showEq (x, t') = BS.unpack x ++ " : " ++ showType t'
