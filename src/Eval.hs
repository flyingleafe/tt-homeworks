{-# LANGUAGE UnicodeSyntax, BangPatterns, TemplateHaskell #-}
module Eval where

import LambdaType
import Prelude.Unicode

modFrees ∷ (N → N) → Λdb → Λdb
modFrees f = modfr 0
    where modfr depth (DVar n) = if n < depth then DVar n else DVar (f n)
          modfr depth (a :@@ b) = modfr depth a :@@ modfr depth b
          modfr depth (Λd e) = Λd $ modfr (depth + 1) e

incFrees, decFrees ∷ Λdb → Λdb
incFrees = modFrees (+1)
decFrees = modFrees (\n → n - 1)

substDB ∷ N → Λdb → Λdb → Λdb
substDB n (DVar m) e = if n ≡ m then e else DVar m
substDB n (a :@@ b) e = substDB n a e :@@ substDB n b e
substDB n (Λd a) e = Λd $ substDB (n + 1) a $ incFrees e

substRdx ∷ Λdb → Λdb → Λdb
substRdx a b = decFrees $ substDB 0 a (incFrees b)

whnf ∷ Λdb → Λdb
whnf ((Λd a) :@@ b) = whnf $ substRdx a b
whnf (a :@@ b) = case whnf a of
                   (Λd e) → whnf $ substRdx e b
                   e → e :@@ b
whnf l = l

normalizeDB ∷ Λdb → Λdb
normalizeDB l = case whnf l of
                  (Λd e) → Λd $ normalizeDB e
                  (a :@@ b) → normalizeDB a :@@ normalizeDB b
                  var → var

normalize ∷ Λ → Λ
normalize = operateDB $ \_ → normalizeDB
