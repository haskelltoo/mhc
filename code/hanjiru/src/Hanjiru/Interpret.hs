module Hanjiru.Interpret where

import Hanjiru.Language (Kata, Move (..))
import Hanjiru.Prelude

import Data.Kind

-- | 

class ParseVia algo where

  -- | Parse an input via the designated algorithm.

  parse ::
        algo info
    ->  Input input t
    =>  Knot (Kata t a)
    ->  input
    ->  Result algo info a

  type Result algo info a :: Type

type State t = Free (Move t)

kataToState :: Kata t a -> State t a
kataToState = runAp liftF
