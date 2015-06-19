{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Inference (infer, showType) where

import Prelude.Unicode
import Utils
import LambdaType
import Equation
import Data.List.Unicode
import Control.Monad.State.Lazy
import Data.Maybe
import Data.Function
import Data.List
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

type Type = EqTerm
type EqPair = (EqSystem, Type)
type TypePair = (Substitution, Type)
type Context = [(VName, EqName)]

{-
   bv       -- context of currently bound variables
   fv       -- context of free variables
   typeVars -- list of used type variables
 -}
data BuildType = BT { bvs ∷ Context
                    , fvs ∷ Context
                    , typeVars ∷ [EqName]}

type EqBuilding = State BuildType

infixr 7 ~>
(~>) ∷ Type → Type → Type
a ~> b = EqFun "->" [a, b]

lookupType ∷ VName → EqBuilding (Maybe VName)
lookupType x = do
  (BT bv fv _) ← get
  return $ lookup x bv <|> lookup x fv

newVar ∷ (VName → BuildType → BuildType) → EqBuilding VName
newVar f = do
  vs ← gets typeVars
  let v = varNotIn vs
  modify $ \bt → bt { typeVars = v:vs }
  modify (f v)
  return v

justVar ∷ EqBuilding VName
justVar = newVar $ const id

newFreeVar ∷ VName → EqBuilding VName
newFreeVar x = newVar $ \v bt → bt { fvs = (x, v) : fvs bt }

newBoundVar ∷ VName → EqBuilding VName
newBoundVar x = newVar $ \v bt → bt { bvs = (x, v) : bvs bt }

removeBoundVar ∷ VName → EqBuilding ()
removeBoundVar x = modify $ \bt →
                   let bv' = deleteBy ((≡) `on` fst) (x, "") $ bvs bt
                   in bt { bvs = bv' }

getTypeOf ∷ VName → EqBuilding VName
getTypeOf x = do
  mt ← lookupType x
  case mt of
    Nothing → newFreeVar x
    Just t → return t

buildSystemM ∷ Λ → EqBuilding EqPair
buildSystemM (Var x) = do
  nv ← getTypeOf x
  return ([], EqVar nv)
buildSystemM (p :@ q) = do
  (ep, tp) ← buildSystemM p
  (eq, tq) ← buildSystemM q
  α ← justVar
  let e = [(tp, tq ~> EqVar α)] ∪ ep ∪ eq
  return (e, EqVar α)
buildSystemM (Λ x p) = do
  α ← newBoundVar x
  (e, t) ← buildSystemM p
  removeBoundVar x
  return (e, EqVar α ~> t)

filterTemp ∷ Context → Context
filterTemp = filter ((≢ "") ∘ fst)

buildSystem ∷ Λ → (EqPair, Context)
buildSystem l = dropRedundant $ runState (buildSystemM l) (BT [] [] [])
    where dropRedundant (r, bt) = (r, filterTemp $ fvs bt)

infer ∷ Λ → Maybe TypePair
infer l = do
  let ((em, tm), ctx) = buildSystem l
  s ← unify em
  let frees = free l
      onlyFrees = map getT frees
      getT x = let v = fromJust $ lookup x ctx in
               let t = fromMaybe (EqVar v) $ lookup v s in
               (x, t)
  return (onlyFrees, substAll s tm)

showType ∷ Type → String
showType (EqVar v) = BS.unpack v
showType (EqFun _ [a, b]) = bracketedF isFun showType a ++ " -> " ++ showType b
    where isFun (EqFun _ _) = True
          isFun _ = False
