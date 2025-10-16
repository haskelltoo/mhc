{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hummingbird.MHC.Var
  (
  -- * Variables
  Var (..),

  -- * Surface names
  Unqualified (..),
  Name,
  InScope,

  -- * Variable lookup
  VarMap,
  insert,
  lookup,
  fromList,
  ) where

import IntMap (IntMap)
import IntMap qualified
import Map (Map)
import Map qualified
import Prelude
import Prettyprinter qualified as Pretty

import HaskellLike.MHC.Name (Unqualified (..))
import Hummingbird.MHC.Builtin

-- |

data Var
  = Var Name Int
  | Prim Builtin
  deriving (Show)

instance Pretty Var where
  pretty = \case
    Var name i ->
      pretty name <> pretty '_' <> pretty i
    Prim builtin ->
      pretty builtin <> pretty '#'

instance Eq Var where
  Var _ i == Var _ j = i == j
  Prim  x == Prim  y = x == y
  _       == _       = False

instance Ord Var where
  compare (Prim  x) (Prim  y) = compare x y
  compare (Prim  _) (Var _ _) = LT
  compare (Var _ i) (Var _ j) = compare i j
  compare (Var _ _) (Prim  _) = GT

-- |

type Name = Unqualified

-- |

type InScope = Map Name Var

-- |

newtype VarMap a = VarMap
  {
    unVarMap :: IntMap a
  }
  deriving (Eq, Ord, Show)
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

-- |

insert :: Var -> a -> VarMap a -> VarMap a
insert (Var _ i) a = VarMap . IntMap.insert i a . unVarMap

-- |

lookup :: Var -> VarMap a -> Maybe a
lookup (Var _ i) = IntMap.lookup i . unVarMap

-- |

fromList :: [(Var, a)] -> VarMap a
fromList = VarMap . IntMap.fromList . map (\(Var _ i, a) -> (i, a))
