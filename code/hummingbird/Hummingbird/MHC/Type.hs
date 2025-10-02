module Hummingbird.MHC.Type where

import Prelude
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

import Hummingbird.MHC.Name

data HbType
  = StackTy Name [HbType]
  | ConTy Name
  | VarTy Name
  | FunTy HbType HbType
  | ConcatTy [HbType]

instance Pretty HbType where
  pretty ty =
    case ty of
      StackTy name tys ->
        Pretty.hsep $
          [
            Pretty.hcat
              [
                pretty name
              , pretty ".."
              ]
          ] ++ map pretty tys
      ConTy name -> pretty name
      VarTy name -> pretty name
      FunTy lhs rhs ->
        Pretty.parens $
          Pretty.hsep
            [
              pretty lhs
            , pretty "->"
            , pretty rhs
            ]
      ConcatTy tys ->
        Pretty.hsep $ map pretty tys
