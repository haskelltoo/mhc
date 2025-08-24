module Hanjiru.Prelude.Free
  (
    -- * Introduction
    -- $intro

    -- * @(~>)@
    type (~>)

    -- * Applicative
  , Ap (..)
  , runAp
  , retractAp
  , freeAp

    -- * Monad
  , Free (..)
  , runFree
  , liftF
  , retract
  )
  where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Kind (Type)

{- $intro

"Hanjiru.Prelude.Free" defines some key /free constructions/.

-}

-- | Maps between two functors. In categorical terms, these are called
--   /natural transformations/.

type f ~> g = forall x. f x -> g x

infixr 0 ~>

-- | The free 'Applicative' (aka monoidal functor) construction.

data Ap f (a :: Type) where

  Pure :: a -> Ap f a

  -- | Apply an argument to a function. The monoid part comes from the fact that we can apply
  --   any number of arguments.

  Ap :: f a -> Ap f (a -> b) -> Ap f b

instance Functor (Ap f) where

  fmap g (Pure a)   = Pure (g a)
  fmap g (Ap pa pf) = Ap pa (fmap (g .) pf)

instance Applicative (Ap f) where

  pure = Pure

  (<*>) :: Ap f (x -> b) -> Ap f x -> Ap f b
  -- rename @x@ so that @pf :: Ap f (a -> x -> b)@ instead of @Ap f (a -> a1 -> b)@.

  Pure f    <*> px = fmap f px
  Ap pa pf  <*> px = Ap pa (flip <$> pf <*> px)

-- | Run @'Ap' f@ by interpreting each node as application in @g@.

runAp :: Applicative g => f ~> g -> Ap f ~> g
runAp (eta :: f ~> g) = go
  where
    go :: Ap f ~> g
    go (Pure a)   = pure a
    go (Ap pa pf) = flip id <$> eta pa <*> go pf

-- | Shortcut: run @'Ap' f@ when @f@ is already an 'Applicative' instance.

retractAp :: Applicative f => Ap f ~> f
retractAp = runAp id

-- | Inject @'Ap' f@ into @'Free' f@.

freeAp :: Functor f => Ap f ~> Free f
freeAp = runAp liftF

-- | The free 'Monad' construction.

data Free f (a :: Type) where

  Return :: a -> Free f a

  Bind :: f a -> (a -> Free f b) -> Free f b

instance Functor (Free f) where

  fmap f (Return a)   = Return (f a)
  fmap f (Bind ma k)  = Bind ma (fmap f . k)

instance Applicative (Free f) where

  pure = Return

  (<*>) = ap

instance Monad (Free f) where

  Return a  >>= k = k a
  Bind ma h >>= k = Bind ma (h >=> k)

-- | Run @'Free' f@ given a natural tranformation @f -> m@. The usual intuition here is that
--   we're interpreting a series of commands in the monad @m@.

runFree :: Monad m => f ~> m -> Free f ~> m
runFree (eta :: f ~> m) = go
  where
    go :: Free f ~> m
    go (Return a)   = pure a
    go (Bind ma k)  = eta ma >>= go . k

-- | @lift@ a functor.

liftF :: Functor f => f ~> Free f
liftF ma = Bind ma Return

-- | Shortcut: run @'Free' f@ when @f@ is already a 'Monad' instance.

retract :: Monad f => Free f ~> f
retract (Return a)  = pure a
retract (Bind ma k) = ma >>= retract . k
