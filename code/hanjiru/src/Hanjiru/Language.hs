module Hanjiru.Language
  (
    (<?>)
  , def
  , Def (..)
  , terminal
  , token
  , expect
  , Knot
  , runKnot
  )
  where

import Knot

import Prelude
import Control.Applicative
import Data.Functor.Identity
import Data.String

infixr 0 <?>

(<?>) :: info -> [Def r info t a] -> Knot (Def r info t a)
(<?>) = def

def :: info -> [Def r info t a] -> Knot (Def r info t a)
def info alts = Bind (Def info alts $ pure id) Tie

data Def r info t a where

  PureDef :: a -> Def r info t a

  Def ::
        info
    ->  [Def r info t a]
    ->  Def r info t (a -> b)
    ->  Def r info t b

  Term ::
        info
    ->  (t -> Maybe a)
    ->  Def r info t (a -> b)
    ->  Def r info t b

  -- Named :: info -> Syn r info t a -> Syn r info t a

  -- Alts :: [Syn r info t a] -> Syn r info t (a -> b) -> Syn r info t a

  -- NonTerm :: r info t a -> Def r info t (a -> b) -> Def r info t a

terminal :: info -> (t -> Maybe a) -> Def r info t a
terminal info t = Term info t (pure id)

instance Functor (Def r info t) where

  fmap f (PureDef a)      = PureDef (f a)
  fmap f (Def info r p)   = Def info r (fmap (f .) p)
  fmap f (Term info t p)  = Term info t (fmap (f .) p)
  -- fmap f (Named info p) = Named info (fmap f p)
  -- fmap f (Alts alts p)  = Alts alts (fmap (f .) p)
  -- fmap f (NonTerm r p)  = NonTerm r (fmap (f .) p)

instance Applicative (Def r info t) where

  pure = PureDef

  PureDef f     <*> q = fmap f q
  Def info r p  <*> q = Def info r (flip <$> p <*> q)
  Term info t p <*> q = Term info t (flip <$> p <*> q)
  -- Named info p  <*> q = Named info (p <*> q)
  -- Alts alts p   <*> q = smartAlts alts (flip <$> p <*> q)
  -- NonTerm r p   <*> q = NonTerm r (flip <$> p <*> q)

token :: (info ~ t, Eq t) => t -> Def r info t t
token x = expect x (== x)

expect :: info -> (t -> Bool) -> Def r info t t
expect info p = terminal info f
  where
    f t | p t = Just t
    f _       = Nothing

instance (info ~ t, a ~ t, Eq t, IsString t) => IsString (Def r info t a) where

  fromString = token . fromString

{-
smartAlts :: [Syn r info t a] -> Syn r info t (a -> b) -> Syn r info t b
smartAlts = \alts p ->
    case alts >>= go of
      [] -> Alts [] (pure id)
      [alt] -> alt <**> p
      alts' -> Alts alts' p
  where
    go (Named info p) = Named info <$> go p
    go (Alts [] _) = []
    go (Alts alts (PureSyn f)) = fmap f <$> alts
    go a = [a]

newtype NT r info t a = NT (Syn' r info t a)

type Syn' r info t a = Syn (NT r) info t a
-}
