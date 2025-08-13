module Hanjiru.Interpret.LR0.Fast where

import Hanjiru.Interpret qualified
import Hanjiru.Language
import Hanjiru.Prelude

import Prelude

data FastLR0 info = FastLR0

instance Hanjiru.Interpret.ParseVia FastLR0 where

  parse FastLR0 = error "unimplemented"

  type Result FastLR0 info a = Either info a
