module Hummingbird.MHC.Bind where

import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

import Hummingbird.MHC.Expr (HbExpr)
import Hummingbird.MHC.Name

data HbBind = Bind Name HbExpr

instance Pretty HbBind where
  pretty (Bind name body) =
    Pretty.hsep
      [
        pretty name
      , Pretty.equals
      , pretty body
      ]
