{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

import Utils
import Parser
import Equation
import System.IO
import qualified Data.ByteString.Char8 as BS

main ∷ IO ()
main = fileIO "task5.in" "task5.out" $ \inp outp → do
         input ← BS.hGetContents inp
         let system = fromRight $ parseEquations input
             solution = unify system
             answer = maybe "Система несовместна" showEqs solution
         hPutStrLn outp answer
