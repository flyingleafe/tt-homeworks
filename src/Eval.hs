{-# LANGUAGE UnicodeSyntax, BangPatterns #-}
module Eval where

import LambdaType
import Prelude.Unicode
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Debug.Trace

type Memo = M.Map Λdb Λdb
type Evaluation = State Memo

getMem ∷ Λdb → Evaluation (Maybe Λdb)
getMem l = gets (M.lookup l)

putMem ∷ Λdb → Λdb → Evaluation ()
putMem k v = modify (M.insert k v)

eval ∷ Λdb → Evaluation Λdb
eval v@(DVar _) = return v
eval (Λd e) = eval e >>= return ∘ Λd
eval rdx@((Λd a) :@@ b) = do
    e ← getMem rdx
    case e of
      Nothing → do
             e' ← eval $ substRdx a b
             putMem rdx e'
             return e'
      Just e' → return e'

eval (a :@@ b) = do
  !a' ← eval a
  case a' of
    (Λd _) → eval $ a' :@@ b
    _ → do
      !b' ← eval b
      return $ a' :@@ b'

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

normalizeDB ∷ Λdb → Λdb
-- normalizeDB l = fst $ runState (eval l) M.empty
normalizeDB e@(DVar _) = e
normalizeDB (Λd e) = Λd $ normalizeDB e
normalizeDB ((Λd e) :@@ a) = normalizeDB $ substRdx e a
normalizeDB (a :@@ b) =
    case normalizeDB a of
      (Λd e) → normalizeDB $ substRdx e b
      e → e :@@ normalizeDB b

normalize ∷ Λ → Λ
normalize = operateDB $ \_ → normalizeDB
