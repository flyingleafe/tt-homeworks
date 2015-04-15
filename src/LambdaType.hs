{-# LANGUAGE UnicodeSyntax #-}
module LambdaType where

import Data.ByteString.Char8
import Utils

type VName = ByteString

data Λ = Var VName
       | Λ VName Λ
       | Λ :@ Λ deriving (Eq, Ord)

type N = Integer
data Λdb = DVar N
            | Λd Λdb
            | Λdb :@@ Λdb

instance Show Λ where
    show (Var v) = unpack v
    show l@(Λ v e) = "\\" ++ unpack v ++ "." ++ bracketed (>) e l
    show a@(x :@ y) = bracketed isLam x a ++ " " ++ bracketed isApp y a
        where isLam (Λ _ _) _ = True
              isLam _ _ = False
              isApp (_ :@ _) _ = True
              isApp _ _ = False
