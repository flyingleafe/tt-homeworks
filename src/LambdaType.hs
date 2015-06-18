{-# LANGUAGE UnicodeSyntax #-}
module LambdaType where

import Prelude.Unicode
import qualified Data.ByteString.Char8 as BS
import Data.List.Unicode
import Data.Maybe
import Utils

-- Ordinary lambdas --
type VName = BS.ByteString

data Λ = Var VName
       | Λ VName Λ
       | Λ :@ Λ
         deriving (Eq, Ord)

free ∷ Λ → [VName]
free (Var v) = [v]
free (Λ v e) = filter (not ∘ (≡ v)) $ free e
free (a :@ b) = free a ∪ free b

-- Lambdas in DeBrujin notation --
type N = Integer

data Λdb = DVar N
         | Λd Λdb
         | Λdb :@@ Λdb
           deriving (Eq, Ord)

-- Isomorphism between Λ and Λdb --

type DContext = [(VName, N)]

getN ∷ VName → DContext → N
getN v = fromJust ∘ lookup v

getV ∷ N → DContext → VName
getV n = fromJust ∘ revlookup n

increment ∷ DContext → DContext
increment = map $ \(v, n) → (v, n + 1)

alphasWithTicks ∷ [VName]
alphasWithTicks = map BS.pack $ concat $ iterate (map (++ "\'")) alphas
  where alphas = map (:[]) ['a'..'z']

varNotIn ∷ [VName] → VName
varNotIn ctx = head $ filter (not ∘ inCtx) $ alphasWithTicks
    where inCtx v = v ∈ ctx

toDB ∷ DContext → Λ → Λdb
toDB ctx (Var v) = DVar $ getN v ctx
toDB ctx (a :@ b) = toDB ctx a :@@ toDB ctx b
toDB ctx (Λ v e) = Λd $ toDB newCtx e
    where newCtx = (v, 0) : increment ctx

fromDB ∷ DContext → Λdb → Λ
fromDB ctx (DVar n) = Var $ getV n ctx
fromDB ctx (a :@@ b) = fromDB ctx a :@ fromDB ctx b
fromDB ctx (Λd e) = Λ v $ fromDB newCtx e
    where v = varNotIn $ map fst ctx
          newCtx = (v, 0) : increment ctx

makeContext ∷ Λ → DContext
makeContext = enumerate ∘ free
    where enumerate ls = zip ls [1..]

operateDB ∷ (DContext → Λdb → Λdb) → Λ → Λ
operateDB f l = fromDB ctx $ f ctx lbd
    where ctx = makeContext l
          lbd = toDB ctx l

-- Useful instances --

instance Show Λ where
    show (Var v) = BS.unpack v
    show (Λ v e) = "\\" ++ BS.unpack v ++ "." ++ show e
    show (x :@ y) = bracketed isLam x ++ " " ++ bracketed isAppOrLam y
        where isLam (Λ _ _) = True
              isLam _ = False
              isAppOrLam (Var _) = False
              isAppOrLam _ = True

instance Show Λdb where
    show (DVar n) = show n
    show (Λd e) = "(\\" ++ show e ++ ")"
    show (x :@@ y) = "(" ++ show x ++ " " ++ show y ++ ")"
