{-# LANGUAGE OverloadedStrings #-}

module Hummingbird.MHC.Builtin where

import Map (Map)
import Map qualified
import Prelude

import HaskellLike.MHC.Name

data Builtin
  = Cat
  | Apply
  | Dip
  | Swap
  | Dup
  | Drop
  | K
  | Cake
  | Placeholder
  deriving (Eq, Ord, Show)

instance Pretty Builtin where
  pretty = \case
    Cat -> pretty @String "cat"
    Apply -> pretty @String "apply"
    Dip -> pretty @String "dip"
    Swap -> pretty @String "swap"
    Dup -> pretty @String "dup"
    Drop -> pretty @String "drop"
    K -> pretty @String "k"
    Cake -> pretty @String "cake"
    Placeholder -> pretty @String "placeholder"

builtins :: Map Unqualified Builtin
builtins = Map.fromList
  [
    (Unqualified "cat", Cat)
  , (Unqualified "apply", Apply)
  , (Unqualified "dip", Dip)
  , (Unqualified "swap", Swap)
  , (Unqualified "dup", Dup)
  , (Unqualified "drop", Drop)
  , (Unqualified "k", K)
  , (Unqualified "cake", Cake)
  , (Unqualified "placeholder", Placeholder)
  ]
