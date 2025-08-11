module Hanjiru.Interpret.LR0.Fast where

import Hanjiru.Input
import Hanjiru.Interpret qualified
import Hanjiru.Language

import Prelude

data FastLR0 = FastLR0

instance Hanjiru.Interpret.ParseVia FastLR0 where

  parse FastLR0 = error "unimplemented"

  type Result FastLR0 info a = Either info a
