{-# LANGUAGE OverloadedStrings, UnicodeSyntax, NoImplicitPrelude #-}
module Parser where

import Prelude hiding (takeWhile)
import Prelude.Unicode
import Data.Monoid.Unicode
import LambdaType
import Equation
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Utils
import qualified Data.ByteString.Char8 as BS
import Debug.Trace

traceM ∷ Monad m ⇒ String → m ()
traceM s = return $ trace s ()

-- Lambdas --

spaces1 = takeWhile1 (≡ ' ')

lexeme, parens ∷ Parser a → Parser a
lexeme p = p <* skipWhile (≡ ' ')
parens p = lexeme (char '(') *> lexeme p <* char ')'

comma = lexeme $ char ','

almostVarname ∷ Parser BS.ByteString
almostVarname = takeWhile $ isOk
    where isOk c = isAlpha_ascii c ∨ isDigit c ∨ c ≡ '\''

prependedVarname ∷ Parser Char → Parser BS.ByteString
prependedVarname p = do
  c ← p
  rest ← almostVarname
  return $ BS.cons c rest

varname ∷ Parser BS.ByteString
varname = prependedVarname letter_ascii

expr, lambda, atom, var ∷ Parser Λ
expr = do
  as ← atom `sepBy1` spaces1
  return $ foldl1 (:@) as

lambda = do
  lexeme $ char '\\'
  v ← lexeme varname
  lexeme $ char '.'
  e ← expr
  return $ Λ v e

atom = var <|> lambda <|> parens expr

var = Var <$> varname

apply ∷ Parser Λ → Parser Λ → Parser Λ
apply p q = do
  e1 ← p <* spaces1
  e2 ← q
  return $ e1 :@ e2

parseExpr ∷ BS.ByteString → Either String Λ
parseExpr = parseOnly expr

substExpr ∷ Parser (VName, Λ)
substExpr = do
  lexeme $ string "["
  v ← lexeme varname
  lexeme $ string ":="
  e ← lexeme expr
  string "]"
  return (v, e)

-- Equations --

eqsystem ∷ Parser EqSystem
eqsystem = equation `sepBy1` endOfLine

equation ∷ Parser Equation
equation = do
  t1 ← lexeme term
  lexeme $ string "="
  t2 ← term
  return (t1, t2)

term ∷ Parser EqTerm
term = function <|> eqvar

function ∷ Parser EqTerm
function = do
  name ← funcname
  args ← parens $ term `sepBy1` comma
  return $ EqFun name args

funcname ∷ Parser BS.ByteString
funcname = prependedVarname $ satisfy $ inClass ['a'..'h']

eqvar ∷ Parser EqTerm
eqvar = eqvarname >>= return ∘ EqVar

eqvarname ∷ Parser BS.ByteString
eqvarname = prependedVarname $ satisfy $ inClass ['i'..'z']

parseEquations ∷ BS.ByteString → Either String EqSystem
parseEquations = parseOnly eqsystem

-- Test function --

readl ∷ String → Λ
readl = fromRight ∘ parseExpr ∘ BS.pack
