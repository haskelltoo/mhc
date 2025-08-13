module Hanjiru.Interpret.LR0.Slow where

import Hanjiru.Interpret qualified
import Hanjiru.Language
import Hanjiru.Prelude

data SlowLR0 info = SlowLR0

instance Hanjiru.Interpret.ParseVia SlowLR0 where

  parse SlowLR0 = error "unimplemented"

  type Result SlowLR0 info a = Either info a


