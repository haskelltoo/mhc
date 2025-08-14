{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds    #-}

module Hanjiru.Prelude.Free
  (
    -- * Free Applicatives (aka Monoidal Functors)
    Ap (..)
  , runAp
  , retractAp
  , freeAp

    -- * Free Monads
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

  Pure f    <*> px = fmap f px
  Ap pa pf  <*> px = Ap pa (flip <$> pf <*> px)
  
  -- The following instance sig doesn't serve any crucial function. It's just here to
  -- alpha-rename the @x@ type variable.
  -- 
  -- Without it:
  --  * @px :: Ap f a@;
  --  * @pa :: f a@;
  --  * @pf :: Ap f (a -> a1 -> b)@, which @a@ is which?!
  --
  -- With it:
  --  * @px :: Ap f x@;
  --  * @pa :: f a@;
  --  * @pf :: Ap f (a -> x -> b)@, much clearer, especially when looking at inferences.

  (<*>) :: Ap f (x -> b) -> Ap f x -> Ap f b

-- | Run @'Ap' f@ by interpreting each node as application in @g@.

runAp :: Applicative g => (forall x. f x -> g x) -> Ap f a -> g a
runAp (eta :: forall x. f x -> g x) = go
  where
    go :: forall a. Ap f a -> g a
    go (Pure a)   = pure a
    go (Ap pa pf) = flip id <$> eta pa <*> go pf

-- | Shortcut: run @'Ap' f@ when @f@ is already an 'Applicative' instances.

retractAp :: Applicative f => Ap f a -> f a
retractAp = runAp id

-- | Inject @'Ap' f@ into @'Free' f@.

freeAp :: Functor f => Ap f a -> Free f a
freeAp = runAp liftF

-- | The free 'Monad' construction.

data Free f (a :: Type) where

  Return :: a -> Free f a

  Bind :: f a -> (a -> Free f b) -> Free f b

instance Functor f => Functor (Free f) where

  fmap f (Return a)   = Return (f a)
  fmap f (Bind ma k)  = Bind ma (fmap f . k)

instance Functor f => Applicative (Free f) where

  pure = Return

  (<*>) = ap

instance Functor f => Monad (Free f) where

  Return a  >>= k = k a
  Bind ma h >>= k = Bind ma (h >=> k)

-- | Run @'Free' f@ given a natural tranformation @f -> m@. The usual intuition here is that
--   we're interpreting a series of commands in the monad @m@.

runFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
runFree (eta :: forall x. f x -> m x) = go
  where
    go :: forall a. Free f a -> m a
    go (Return a)   = pure a
    go (Bind ma k)  = eta ma >>= go . k

-- | @lift@ a functor.

liftF :: Functor f => f a -> Free f a
liftF ma = Bind ma Return

-- | Shortcut: run @'Free' f@ when @f@ is already a 'Monad' instance.

retract :: Monad f => Free f a -> f a
retract (Return a)  = pure a
retract (Bind ma k) = ma >>= retract . k
