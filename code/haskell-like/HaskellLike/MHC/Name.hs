module HaskellLike.MHC.Name where

import Prelude (Show)
import Data.Char (isLetter)
import Data.Eq
import Data.Ord
import Data.Text (Text)
import Prettyprinter

data Unqualified = Unqualified Text
  deriving (Eq, Ord, Show)

instance Pretty Unqualified where
  pretty (Unqualified name) = pretty name
