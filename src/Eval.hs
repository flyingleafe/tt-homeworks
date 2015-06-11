{-# LANGUAGE UnicodeSyntax #-}
module Eval where

import LambdaType
import Prelude.Unicode
import Data.Maybe
import Control.Monad.State.Lazy
import qualified Data.Map as M

type Memo = M.Map Λdb Λdb
type Evaluation = State Memo

getMem ∷ Λdb → Evaluation (Maybe Λdb)
getMem l = gets (M.lookup l)

putMem ∷ Λdb → Λdb → Evaluation ()
putMem k v = modify (M.insert k v)

eval ∷ Λdb → Evaluation Λdb
eval l = liftM2 fromMaybe evalAndPut (getMem l)
    where evalAndPut = do
            e ← eval' l
            putMem l e
            return e

eval' ∷ Λdb → Evaluation Λdb
eval' ((Λd a) :@@ b) = eval $ substDB 0 (decFrees a) b
eval' v@(DVar _) = return v
eval' (a :@@ b) = do
  a' ← eval a
  case a' of
    (Λd _) → eval $ a' :@@ b
    _ → do
      b' ← eval b
      return $ a' :@@ b'
eval' (Λd e) = eval e >>= return ∘ Λd

modFrees ∷ (N → N) → Λdb → Λdb
modFrees f = modfr 0
    where modfr depth (DVar n) = if n ≤ depth then DVar n else DVar (f n)
          modfr depth (a :@@ b) = modfr depth a :@@ modfr depth b
          modfr depth (Λd e) = Λd $ modfr (depth + 1) e

incFrees, decFrees ∷ Λdb → Λdb
incFrees = modFrees (+1)
decFrees = modFrees (\n → n - 1)

substDB ∷ N → Λdb → Λdb → Λdb
substDB n (DVar m) e = if n ≡ m then e else DVar m
substDB n (a :@@ b) e = substDB n a e :@@ substDB n b e
substDB n (Λd a) e = Λd $ substDB (n + 1) a $ incFrees e

normalizeDB ∷ Λdb → Λdb
normalizeDB l = fst $ runState (eval l) M.empty

normalize ∷ Λ → Λ
normalize l = fromDB ctx $ normalizeDB ldb
    where ctx = makeContext l
          ldb = toDB ctx l
