{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
module Utils where

import Prelude.Unicode
import System.IO
import System.IO.Unsafe
import Data.String

-- mama ama criminal
instance IsString Handle where
    fromString = unsafePerformIO . flip openFile ReadWriteMode

revlookup ∷ Eq b ⇒ b → [(a, b)] → Maybe a
revlookup _ [] = Nothing
revlookup b (p:ps) = if b ≡ snd p then Just (fst p) else revlookup b ps

fromRight ∷ Either a b → b
fromRight (Right a) = a
fromRight (Left _) = error "Left value"

pairM ∷ Monad m ⇒ (m a, m b) → m (a, b)
pairM (f, g) = do
  a ← f
  b ← g
  return (a, b)

bracketed ∷ Show a ⇒ (a → t → Bool) → a → t → String
bracketed comp a e
    | a `comp` e = "(" ++ show a ++ ")"
    | otherwise = show a

fileIO ∷ Handle → Handle → (Handle → Handle → IO c) → IO c
fileIO inp outp action = do
  res ← action inp outp
  hClose inp
  hClose outp
  return res
