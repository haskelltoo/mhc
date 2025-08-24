module Hanjiru.Interpret.LR0.Slow where

import Hanjiru.Prelude
import Hanjiru.Interpret qualified
import Hanjiru.Language

import Data.Kind

data SlowLR0 (f :: Type -> Type) info = SlowLR0

instance Hanjiru.Interpret.ParseVia SlowLR0 where

  parse SlowLR0 = error "unimplemented"

  type Result SlowLR0 info a = Either info a


