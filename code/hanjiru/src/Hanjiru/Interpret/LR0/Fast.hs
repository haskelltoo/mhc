module Hanjiru.Interpret.LR0.Fast where

import Hanjiru.Prelude
import Hanjiru.Interpret qualified
import Hanjiru.Language

import Data.Kind (Type)

data FastLR0 (f :: Type -> Type) info = FastLR0

instance Hanjiru.Interpret.ParseVia FastLR0 where

  parse FastLR0 = error "unimplemented"

  type Result FastLR0 info a = Either info a
