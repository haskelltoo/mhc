module Language.Lambek.Ident where

import Prelude
import Data.Text

newtype Ident = Ident Text
  deriving (Show)
