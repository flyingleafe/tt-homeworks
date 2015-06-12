{-# LANGUAGE OverloadedStrings, UnicodeSyntax, NoImplicitPrelude #-}
module Parser where

import Prelude hiding (takeWhile)
import Prelude.Unicode
import Data.Monoid.Unicode
import LambdaType
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Utils
import qualified Data.ByteString.Char8 as BS

spaces1 = takeWhile1 (≡ ' ')

lexeme, parens ∷ Parser a → Parser a
lexeme p = p <* skipWhile (≡ ' ')
parens p = lexeme (string "(") *> lexeme p <* string ")"

varname ∷ Parser BS.ByteString
varname = do
  letters ← takeWhile1 isAlpha_ascii
  digits ← takeWhile isDigit
  ticks ← takeWhile (≡ '\'')
  return $ letters ⊕ digits ⊕ ticks

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

-- Test function --

readl ∷ String → Λ
readl = fromRight ∘ parseExpr ∘ BS.pack
