module Hummingbird.MHC.Expr where

import Prelude
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

import Hummingbird.MHC.Name

data HbExpr
  = Word Name
  | Lambda Name HbExpr
  | Match [Alt]
  | Quoted HbExpr
  | Concat [HbExpr]

instance Pretty HbExpr where
  pretty expr =
    case expr of
      Word word -> pretty word
      Lambda arg body ->
        Pretty.hcat
          [
            Pretty.backslash
          , Pretty.hsep
              [
                pretty arg
              , pretty "->"
              , pretty body
              ]
          ]
      Match alts -> pretty alts
      Quoted quoted ->
        Pretty.brackets $ pretty quoted
      Concat xs ->
        Pretty.hsep $ map pretty xs

data Alt

instance Pretty Alt where
  pretty = pretty
