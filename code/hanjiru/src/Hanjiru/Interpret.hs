module Hanjiru.Interpret
  (
    ParseVia (..)
  , module Hanjiru.Input
  )
  where

import Hanjiru.Input
import Hanjiru.Language (Def)
import Knot

import Prelude
import Data.Kind

-- | 

class ParseVia algo where

  -- | Parse an input via the designated algorithm.

  parse ::
        algo
    ->  Input input t
    =>  Knot (Def r info t a)
    ->  input
    ->  Result algo info a

  type Result algo info a :: Type
