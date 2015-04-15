{-# LANGUAGE UnicodeSyntax #-}
module Eval where

import LambdaType
import Substitutions
import Prelude.Unicode
import Data.List.Unicode
import Data.Maybe

type Subst = N → Λdb
type Mapping = VName → N
type RevMapping = N → VName

leaf ∷ Subst
leaf n = DVar n

-- Modify the substitution: increment all the
-- free identificators in result
incv ∷ Integer → Subst → Subst
incv depth sub n = case sub n of
  (DVar n') → if n' ≥ depth then DVar (n' + 1) else DVar n'
  (Λd e) → Λd $ incv (depth + 1) (const e) n
  (a :@@ b) → incv depth (const a) n :@@ incv depth (const b) n

-- Modify all the free identificators
modfree ∷ (N → N) → Integer → Λd6b → Λdb
modfree f depth (DVar n)= if n ≥ depth then DVar (f n) else DVar n
modfree f depth (a :@@ b) = modfree f depth a :@@ modfree f depth b
modfree f depth (Λd e) = Λd $ modfree f (depth + 1) e

decx, incx ∷ Integer → Λdb → Λdb
decx = modfree (flip (-) 1)
incx = modfree (+ 1)

snoc ∷ Subst → N → Λdb → Subst
snoc c n e n' = if n' == n then e else incv 0 c $ n' - 1

eval ∷ Λdb → Subst → Λdb
eval (DVar v) ss = ss v
eval (Λd a) ss = Λd $ eval a $ snoc ss 0 $ DVar 0
eval (a :@@ b) ss = case (eval a ss, eval b ss) of
                      (DVar v, e)    → DVar v :@@ e
                      (Λd a', e)     → decx 1 $ eval a' $ snoc ss 0 $ incx 1 e
                      (a' :@@ b', e) → (a' :@@ b') :@@ e

run e = eval e leaf

makeFreeMap ∷ Λ → Mapping
makeFreeMap e v = fromJust $ lookup v (zip (free e) [0, 1..])

upd ∷ Mapping → VName → N → Mapping
upd m v n v' = if v == v' then n else m v' + 1

revUpd ∷ RevMapping → N → VName → RevMapping
revUpd m n v n' = if n ≡ n' then v else m (n' - 1)

mapToDb ∷ Λ → Mapping → Λdb
mapToDb (Var v) m = DVar $ m v
mapToDb (Λ v e) m = Λd $ mapToDb e $ upd m v 0
mapToDb (a :@ b) m = mapToDb a m :@@ mapToDb b m

toDB ∷ Λ → Λdb
toDB e = mapToDb e $ makeFreeMap e

alphasWithTicks = concat $ iterate (map (++ "\'")) alphas
  where alphas = map (:[]) ['a'..'z']

varNotIn ∷ [VName] → VName
varNotIn vs = head $ filter (not ∘ (∈ vs)) $ alphasWithTicks

mapFromDb ∷ [VName] → Λdb → RevMapping → Λ
mapFromDb _  (DVar n) m  = Var $ m n
mapFromDb vs (Λd e) m    = Λ nv $ mapFromDb (nv:vs) e
                           $ revUpd m 0 nv
    where nv = varNotIn vs
mapFromDb vs (a :@@ b) m = mapFromDb vs a m :@ mapFromDb vs b m

makeFrees ∷ Λdb → ([VName], RevMapping)
makeFrees de = (map snd initMap, \n → fromJust $ lookup n initMap)
    where initMap = zip (freeNums 0 de) alphasWithTicks
          freeNums depth (DVar n) = if n' ≥ 0 then [n'] else []
              where n' = n - depth
          freeNums depth (Λd e) = freeNums (depth + 1) e
          freeNums depth (a :@@ b) = freeNums depth a ∪ freeNums depth b

fromDB ∷ Λdb → Λ
fromDB de = mapFromDb vn de im
    where (vn, im) = makeFrees de
