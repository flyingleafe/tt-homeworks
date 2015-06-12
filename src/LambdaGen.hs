{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Main where

import LambdaType
import Eval
import Utils
import Prelude.Unicode
import Parser
import Data.Attoparsec.ByteString.Char8
import System.IO
import System.Environment
import qualified Data.ByteString as BS
import Debug.Trace

assignment ∷ Parser (VName, Λ)
assignment = do
  name ← lexeme varname
  lexeme $ string "="
  e ← expr
  return (name, e)

assignments ∷ Parser [(VName, Λ)]
assignments = assignment `sepBy` (many1 endOfLine)

substitute ∷ VName → Λ → Λ → Λ
substitute v chto = operateDB substIt
    where substIt ctx kuda = case lookup v ctx of
                               Nothing → kuda
                               Just n → substDB n kuda $ toDB ctx chto

substAll ∷ [(VName, Λ)] → Λ
substAll [] = error "empty list"
substAll [("out", l)] = l
substAll [(_, _)] = error "last statement is not 'out'"
substAll ((name, e) : ps) = substAll substedThat
    where substedThat = map substToPair ps
          substToPair (name', e') = (name', substitute name e e')

main ∷ IO ()
main = do
  args ← getArgs
  if length args < 2 then putStrLn "too few arguments"
  else do
    inp ← openFile (args !! 0) ReadMode
    outp ← openFile (args !! 1) WriteMode
    contents ← BS.hGetContents inp
    let asts = fromRight $ parseOnly assignments contents
        outExp = substAll asts
    hPutStrLn outp $ show outExp
    hClose inp
    hClose outp
