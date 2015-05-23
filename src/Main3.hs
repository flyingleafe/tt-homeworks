{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Main where

import LambdaType
import Substitutions
import Utils
import Parser
import ParserUtils
import Prelude.Unicode
import Data.Monoid.Unicode
import System.IO
import qualified Data.ByteString.Char8 as BS

substIt ∷ (Λ, (VName, Λ)) → String
substIt (kuda, (v, chto)) = case substitute v chto kuda of
                              Just e → show e
                              Nothing → "Нет свободы для подстановки для переменной " ⊕
                                      BS.unpack (head $ notFreeFSVars chto v kuda)

main ∷ IO ()
main = fileIO "task3.in" "task3.out" $ \inp outp →
       hReadEx inp (pairM (lexeme expr, substExpr)) >>= hPutStrLn outp ∘ substIt
