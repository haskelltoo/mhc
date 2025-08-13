module Hanjiru.Prelude.Free where

import Control.Applicative
import Control.Monad
import Data.Function

-- | The free 'Applicative' construction.

data Ap f a where
  Pure  :: a -> Ap f a
  Ap    :: f a -> Ap f (a -> b) -> Ap f b

instance Functor (Ap f) where

  fmap g (Pure a)   = Pure (g a)
  fmap g (Ap mx mf) = Ap mx (fmap (g .) mf)

instance Applicative (Ap f) where

  pure = Pure

  Pure f    <*> ma = fmap f ma
  Ap mx mf  <*> ma = Ap mx (flip <$> mf <*> ma)

runAp :: Applicative g => (forall x. f x -> g x) -> Ap f a -> g a
runAp eta (Pure a)    = pure a
runAp eta (Ap mx mf)  = flip id <$> eta mx <*> runAp eta mf

retractAp :: Applicative f => Ap f a -> f a
retractAp = runAp id

-- | The free 'Monad' construction.

data Free f a where
  Return :: a -> Free f a
  Bind   :: f a -> (a -> Free f b) -> Free f b

instance Functor f => Functor (Free f) where

  fmap f (Return a)   = Return (f a)
  fmap f (Bind ma k)  = Bind ma (fmap f . k)

instance Functor f => Applicative (Free f) where

  pure = Return

  (<*>) = ap

instance Functor f => Monad (Free f) where

  Return a  >>= k = k a
  Bind ma h >>= k = Bind ma (h >=> k)

liftF :: Functor f => f a -> Free f a
liftF ma = Bind ma Return

retract :: Monad f => Free f a -> f a
retract (Return a)  = pure a
retract (Bind ma k) = ma >>= retract . k
