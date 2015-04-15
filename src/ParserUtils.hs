{-# LANGUAGE UnicodeSyntax #-}
module ParserUtils where

import System.IO
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Prelude.Unicode
import Utils

hReadEx ∷ Handle → Parser a → IO a
hReadEx f p = BS.hGetLine f >>= return ∘ fromRight ∘ parseOnly p

readEx :: Parser a -> IO a
readEx = hReadEx stdin
