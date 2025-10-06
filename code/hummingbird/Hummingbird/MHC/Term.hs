module Hummingbird.MHC.Term where

import Prelude.GHC
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

import Hummingbird.MHC.Name

data HbTerm
  = Word Name
  | Lit Integer
  | Lambda Name HbTerm
  | Match [Alt]
  | Quoted HbTerm
  | Concat [HbTerm]

instance Pretty HbTerm where
  pretty expr =
    case expr of
      Word word -> pretty word
      Lit literal -> pretty literal
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
