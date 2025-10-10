{-# LANGUAGE TypeOperators #-}

module Prelude
  (
  -- * Combinators
  ($),
  (&),
  (.),
  fix,
  flip,
  on,
  type (~),

  -- * Maybe
  Maybe (..),
  maybe,

  -- * Either
  Either (..),
  either,

  -- * Identity
  id,
  Identity (..),

  -- * Const
  const,
  Const (..),

  -- * Tuples
  curry, uncurry,
  fst, snd,

  -- * Booleans
  Bool (..),
  (&&),
  (||),
  not,
  otherwise,

  -- * Comparison
  Eq (..),
  Ord (..),
  Ordering (..),
  clamp,
  comparing,

  -- ** Reversing order
  Down (..),

  -- * Numbers
  Bounded (..),
  Enum (..),
  Int,
  Integer,
  Integral (..),
  Num (..),
  Word,
  fromIntegral,
  even, odd,
  gcd, lcm, (^), (^^),

  -- * Real numbers
  Double,
  Float,
  Floating (..),
  Fractional (..),
  Rational,
  Real (..),
  RealFloat (..),
  RealFrac (..),

  -- * Lists
  (++),
  catMaybes,
  concat,
  concatMap,
  filter,
  intercalate,
  intersperse,
  map,
  nub,
  nubBy,
  partition,
  partitionEithers,
  reverse,
  sort,
  sortBy,
  sortOn,
  splitAt,
  Functor.unzip,
  unzip3,
  zip,
  zipWith,
  zip3,
  zipWith3,
  
  -- ** Building lists
  cycle,
  iterate,
  repeat,
  replicate,
  unfoldr,

  -- ** Take and drop
  drop,
  dropWhile,
  take,
  takeWhile,

  -- * Strings
  Char,
  Show (..),
  String,
  Text,
  lines,
  words,
  unlines,
  unwords,

  -- * Folds and traversals
  Foldable (..),
  Traversable (..),
  and,
  or,
  any,
  all,
  foldlM,
  foldrM,

  -- * Monoids and semigroups
  Monoid (..),
  Semigroup (..),

  -- ** Monoids over 'Bool'
  All (..),
  Any (..),

  -- ** Monoids over 'Num'
  Sum (..),
  Product (..),

  -- ** Monoids over 'Maybe'
  First (..),
  Last (..),

  -- ** Monoids over @('->')@
  Dual (..),
  Endo (..),

  -- ** Monoid over 'Applicative'
  Ap (..),

  -- ** Monoid over 'Alternative'
  Alt (..),

  -- * Functors
  Functor (..),
  (<$>), ($>), (<&>),
  void,

  -- ** Bifunctors
  Bifunctor (..),

  -- ** Contravariant functors
  Contravariant (..),
  ($<),
  (>$<),

  -- * Applicative functors
  Applicative (..),
  Alternative (..),
  (<**>),
  asum,
  optional,

  -- * Monads
  Monad (..),
  MonadFail (..),
  MonadPlus (..),
  (=<<),
  (>=>),
  (<=<),
  ap,
  filterM,
  foldM,
  foldM_,
  forM,
  forM_,
  guard,
  join,
  mapM_,
  mfilter,
  msum,
  replicateM,
  replicateM_,
  sequence_,
  when,
  unless,
  zipWithM,
  zipWithM_,

  -- * Input and output
  IO,
  getChar,
  getContents,
  getLine,
  interact,
  print,
  putStr,
  putStrLn,

  -- ** Files
  FilePath,
  appendFile,
  readFile,
  readIO,
  readLn,
  writeFile,

  -- * Runtime errors
  error,
  errorWithoutStackTrace,
  undefined,
  ) where

import Prelude.GHC

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Function
import Data.Functor as Functor
import Data.Functor.Const
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
