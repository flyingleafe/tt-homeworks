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

-- Lambdas --

spaces1 = takeWhile1 (≡ ' ')

lexeme, parens ∷ Parser a → Parser a
lexeme p = p <* skipWhile (≡ ' ')
parens p = lexeme (string "(") *> lexeme p <* string ")"

comma = lexeme $ char ','

(.|.) ∷ (a → Bool) → (a → Bool) → a → Bool
(f .|. g) a = f a ∨ g a

almostVarname ∷ Parser BS.ByteString
almostVarname = takeWhile $ isAlpha_ascii .|. isDigit .|. (≡ '\'')

prependedVarname ∷ Parser Char → Parser BS.ByteString
prependedVarname p = do
  c ← p
  rest ← almostVarname
  return $ BS.cons c rest

varname ∷ Parser BS.ByteString
varname = prependedVarname letter_ascii

expr, app, lambda, atom, var ∷ Parser Λ
expr = app `apply` lambda <|> lambda <|> app

lambda = do
  lexeme $ string "\\"
  v ← lexeme varname
  lexeme $ string "."
  e ← expr
  return $ Λ v e

app = do
  a ← lexeme atom
  as ← atom `sepBy` spaces1
  return $ foldl (:@) a as

atom = parens expr <|> var

var = varname >>= return ∘ Var

apply ∷ Parser Λ → Parser Λ → Parser Λ
apply p q = do
  e1 ← lexeme p
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
