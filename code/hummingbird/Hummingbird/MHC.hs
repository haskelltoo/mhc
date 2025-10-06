module Hummingbird.MHC
  (
  -- * Hummingbird

  -- ** Modules
  HbMod (..),

  -- ** Declarations
  Feather (..),
  HbBind (..),
  
  -- ** Expressions
  HbExpr (..),
  Alt (..),

  -- ** Types
  HbType (..),
  ) where

import Prelude.GHC
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

import Hummingbird.MHC.Bind
import Hummingbird.MHC.Expr
import Hummingbird.MHC.Feathers
import Hummingbird.MHC.Name
import Hummingbird.MHC.Type

data HbMod = HbMod Name [Feather]

instance Pretty HbMod where
  pretty (HbMod name feathers) =
    Pretty.vcat
      [
        Pretty.hsep
          [
            pretty name
          , pretty "module"
          ]
      , Pretty.vcat $ pretty <$> feathers
      ]
