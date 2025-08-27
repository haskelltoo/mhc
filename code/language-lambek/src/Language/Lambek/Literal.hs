module Language.Lambek.Literal where

import Internal.Prelude

data Literal
  = LInt Int
  | LInt64 Int64
  | LInteger Integer
  | LDouble Double
  | LFloat Float
  | LRat Rational
  | LChar Char
  | LStr String
  | LBStr String            -- bytestring
  | LPrim String
  | LExn String             -- exception to raise
  | LTick String
  deriving (Eq, Show)
