module Hanjiru.Interpret.LR0.Slow where

import Hanjiru.Input
import Hanjiru.Interpret qualified
import Hanjiru.Language

import Prelude

data SlowLR0 = SlowLR0

instance Hanjiru.Interpret.ParseVia SlowLR0 where

  parse SlowLR0 = error "unimplemented"

  type Result SlowLR0 info a = Either info a
