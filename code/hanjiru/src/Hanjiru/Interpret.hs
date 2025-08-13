module Hanjiru.Interpret
  (
    ParseVia (..)
  , module Hanjiru.Prelude
  )
  where

import Hanjiru.Language (Form)
import Hanjiru.Prelude (Input, Knot)

import Data.Kind

-- | 

class ParseVia algo where

  -- | Parse an input via the designated algorithm.

  parse ::
        algo info
    ->  Input input t
    =>  Knot (Form t a)
    ->  input
    ->  Result algo info a

  type Result algo info a :: Type
