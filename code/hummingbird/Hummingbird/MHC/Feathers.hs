module Hummingbird.MHC.Feathers where

import Prelude
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

import Hummingbird.MHC.Bind (HbBind)

-- | 'Feather's are the topmost fragments of a Hummingbird program.

data Feather
  = Defn HbBind

instance Pretty Feather where
  pretty feather =
    case feather of
      Defn bind -> pretty bind

instance Show Feather where
  show = show . pretty
