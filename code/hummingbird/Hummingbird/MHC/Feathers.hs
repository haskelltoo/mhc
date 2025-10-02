module Hummingbird.MHC.Feathers where

import Prelude
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

import Hummingbird.MHC.Bind (HbBind)
import Hummingbird.MHC.Name
import Hummingbird.MHC.Type (HbType)

-- | 'Feather's are the topmost fragments of a Hummingbird program.

data Feather
  = Defn HbBind
  | Sig Name HbType

instance Pretty Feather where
  pretty feather =
    case feather of
      Defn bind -> pretty bind
      Sig name ty ->
        Pretty.hsep
          [
            pretty name
          , Pretty.colon
          , pretty ty
          ]

instance Show Feather where
  show = show . pretty
