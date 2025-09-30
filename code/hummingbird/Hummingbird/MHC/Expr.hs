module Hummingbird.MHC.Expr where

import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

import Hummingbird.MHC.Name

data HbExpr
  = Push HbVal
  | Word Name
  | Lambda Name HbExpr
  | Compose HbExpr HbExpr
  | Match [Alt]

instance Pretty HbExpr where
  pretty expr =
    case expr of
      Push val -> pretty val
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
      Compose lhs rhs ->
        Pretty.hsep
          [
            pretty lhs
          , pretty rhs
          ]
      Match alts -> pretty alts

data Alt

instance Pretty Alt where
  pretty = pretty

data HbVal
  = Var Name
  | Quotation HbExpr

instance Pretty HbVal where
  pretty val =
    case val of
      Var name -> pretty name
      Quotation quote -> Pretty.brackets (pretty quote)
