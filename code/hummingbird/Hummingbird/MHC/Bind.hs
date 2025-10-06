module Hummingbird.MHC.Bind where

import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

import Hummingbird.MHC.Name
import Hummingbird.MHC.Term (HbTerm)

data HbBind = Bind Name HbTerm

instance Pretty HbBind where
  pretty (Bind name body) =
    Pretty.hsep
      [
        pretty name
      , Pretty.equals
      , pretty body
      ]
