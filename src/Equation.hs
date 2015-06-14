{-# LANGUAGE UnicodeSyntax, OverloadedStrings, FlexibleInstances, TypeSynonymInstances #-}
module Equation ( EqName
                , EqTerm(..)
                , Equation
                , EqSystem
                , Substitution
                , unify
                , showEqs)
where

import Prelude.Unicode
import Data.List
import Data.List.Unicode
import qualified Data.ByteString.Char8 as BS

type EqName = BS.ByteString

data EqTerm = EqVar EqName
            | EqFun EqName [EqTerm]
              deriving (Eq, Ord)

type Equation = (EqTerm, EqTerm)
type EqSystem = [Equation]
type Substitution = [(EqName, EqTerm)]

unify ∷ EqSystem → Maybe Substitution
unify [] = Just []
unify ((a, b) : eqs) = do
  s ← unify eqs
  neq ← unifyEq (apply s a, apply s b)
  return $ neq ∪ s

unifyEq ∷ Equation → Maybe Substitution
unifyEq (EqVar a, EqVar b) =
    if a ≡ b
    then Just []
    else Just [(a, EqVar b)]
unifyEq (EqVar a, f@(EqFun _ _)) = checkOccurs a f
unifyEq (f@(EqFun _ _), EqVar a) = checkOccurs a f
unifyEq (f@(EqFun fname ps), g@(EqFun gname qs)) =
    if f ≡ g then Just []
    else if fname ≢ gname ∨ length ps ≢ length qs then Nothing
         else unify $ zip ps qs

checkOccurs ∷ EqName → EqTerm → Maybe Substitution
checkOccurs a f = if a `occursIn` f then Nothing else Just [(a, f)]

occursIn ∷ EqName → EqTerm → Bool
v `occursIn` (EqVar t) = v ≡ t
v `occursIn` (EqFun _ ts) = any (occursIn v) ts

apply ∷ Substitution → EqTerm → EqTerm
apply s t = foldr (uncurry subst) t s

subst ∷ EqName → EqTerm → EqTerm → EqTerm
subst v t (EqVar v') = if v ≡ v' then t else EqVar v'
subst v t (EqFun f ts) = EqFun f $ map (subst v t) ts

instance Show EqTerm where
    show (EqVar v) = BS.unpack v
    show (EqFun f ts) = BS.unpack f ++ "(" ++ intercalate ", " (map show ts) ++ ")"

showEqs ∷ Substitution → String
showEqs eqs = unlines $ map showEq eqs
    where showEq (a, b) = BS.unpack a ++ " = " ++ show b
