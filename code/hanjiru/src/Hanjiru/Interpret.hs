module Hanjiru.Interpret where

import Hanjiru.Prelude
import Hanjiru.Language (Kata, Move (..))

import Data.Kind

-- | 

class ParseVia algo where

  -- | Parse an input via the designated algorithm.

  parse ::
        algo f info
    ->  Input input t
    =>  Knot f (Kata t a)
    ->  input
    ->  Result algo info a

  type Result algo info a :: Type

type State t = Free (Move t)
