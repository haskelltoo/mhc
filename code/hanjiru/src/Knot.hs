{-# LANGUAGE PolyKinds #-}

module Knot where

import Prelude
import Control.Monad
import Control.Monad.Fix
import Data.Kind

-- | 'Knot' is a free construction for 'MonadFix'.

-- | Note that 'Knot' is not quite the general free monad construction for an endofunctor,
--   since it clearly does not take an endofunctor argument.
--   Instead, we have a definition that is /good enough/ for our applications of it.

data Knot (a :: k) :: Type where

  Tie :: a -> Knot a

  Knot :: (a -> Knot a) -> (a -> Knot b) -> Knot b

  Bind :: a -> (a -> Knot b) -> Knot b

instance Functor Knot where

  fmap f (Tie a)    = Tie (f a)
  fmap f (Knot g h) = Knot g (fmap f . h)
  fmap f (Bind p h) = Bind p (fmap f . h)

instance Applicative Knot where

  pure = Tie

  (<*>) = ap

instance Monad Knot where

  Tie a     >>= k = k a
  Knot g h  >>= k = Knot g (h >=> k)
  Bind p h  >>= k = Bind p (h >=> k)

instance MonadFix Knot where

  mfix f = Knot f Tie

-- | Interpret a 'Knot' in a monad @m@ by way of a natural transformation.

runKnot :: MonadFix m => (forall a. a -> m a) -> Knot b -> m b

runKnot _ (Tie a) = pure a

runKnot eta (Bind a k) =
  runKnot eta . k =<< eta a

runKnot eta (Knot f k) =
  runKnot eta . k =<< mfix (runKnot eta <$> f)
