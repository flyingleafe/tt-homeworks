{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
module Utils where

import Prelude.Unicode
import System.IO

revlookup ∷ Eq b ⇒ b → [(a, b)] → Maybe a
revlookup _ [] = Nothing
revlookup b (p:ps) = if b ≡ snd p then Just (fst p) else revlookup b ps

pairlookup ∷ Eq a ⇒ a → [(a, b)] → Maybe (a, b)
pairlookup _ [] = Nothing
pairlookup a (p:ps) = if a ≡ fst p then Just p else pairlookup a ps

fromRight ∷ Either a b → b
fromRight (Right a) = a
fromRight (Left _) = error "Left value"

pairM ∷ Monad m ⇒ (m a, m b) → m (a, b)
pairM (f, g) = do
  a ← f
  b ← g
  return (a, b)

bracketedF ∷ (a → Bool) → (a → String) → a → String
bracketedF p toS a
    | p a = "(" ++ toS a ++ ")"
    | otherwise = toS a

bracketed ∷ Show a ⇒ (a → Bool) → a → String
bracketed p = bracketedF p show

fileIO ∷ String → String → (Handle → Handle → IO c) → IO c
fileIO inp' outp' action = do
  inp ← openFile inp' ReadMode
  outp ← openFile outp' WriteMode
  res ← action inp outp
  hClose inp
  hClose outp
  return res
