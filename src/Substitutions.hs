{-# LANGUAGE UnicodeSyntax #-}
module Substitutions where

import Prelude.Unicode
import LambdaType
import Data.List
import Data.List.Unicode

boundedNear ∷ VName → Λ → [VName]
boundedNear v = sort ∘ nub ∘ gbn []
    where gbn bnd (Var v') = if v ≡ v' then bnd else []
          gbn bnd (Λ v' e) = gbn (v':bnd) e
          gbn bnd (a :@ b) = gbn bnd a ∪ gbn bnd b

notFreeFSVars ∷ Λ → VName → Λ → [VName]
notFreeFSVars chto v kuda = free chto ∩ boundedNear v kuda

freeForSubst ∷ Λ → VName → Λ → Bool
freeForSubst chto v kuda = notFreeFSVars chto v kuda ≡ []

subst ∷ VName → Λ → Λ → Λ
subst v e (Var v')
    | v ≡ v' = e
    | otherwise = Var v'
subst v e (Λ v' e')
    | v ≢ v' = Λ v' $ subst v e e'
    | otherwise = Λ v' e'
subst v e (a :@ b) = subst v e a :@ subst v e b

substitute ∷ VName → Λ → Λ → Maybe Λ
substitute v e e'
    | freeForSubst e v e' = Just $ subst v e e'
    | otherwise = Nothing
