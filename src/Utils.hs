{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
module Utils where

import System.IO
import Prelude.Unicode

class InputID a where
    getInput ∷ a → IO Handle

class OutputID a where
    getOutput ∷ a → IO Handle

instance InputID Handle where
    getInput h = do
      isReadable ← hIsReadable h
      if isReadable then return h else error "Handle is not readable"

instance OutputID Handle where
    getOutput h = do
      isWritable ← hIsWritable h
      if isWritable then return h else error "Handle is not writable"

instance Show a ⇒ InputID a where
    getInput = flip openFile ReadMode ∘ show

instance Show a ⇒ OutputID a where
    getOutput = flip openFile WriteMode ∘ show

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

fileIO ∷ (InputID a, OutputID b) ⇒ a → b → (Handle → Handle → IO c) → IO c
fileIO inp outp action = do
  infile ← getInput inp
  outfile ← getOutput outp
  res ← action infile outfile
  hClose infile
  hClose outfile
  return res
