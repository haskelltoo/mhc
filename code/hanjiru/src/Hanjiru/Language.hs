module Hanjiru.Language
  (
    Form (..)
  
  , (<?>)
  , def
  , token
  , expect
  , terminal

  , Knot
  , runKnot
  )
  where

import Hanjiru.Prelude

import Control.Applicative
import Data.Functor.Identity
import Data.String

data Form t a where

  Complete :: a -> Form t a

  Scan :: (t -> Maybe a) -> Form t (a -> b) -> Form t b

  Predict :: [Form t a] -> Form t (a -> b) -> Form t b

infixr 0 <?>

(<?>) :: info -> [Form t a] -> Knot (Form t a)
(<?>) = def

def :: info -> [Form t a] -> Knot (Form t a)
def info alts = Bend (Predict alts $ pure id) Tie

terminal :: info -> (t -> Maybe a) -> Form t a
terminal info t = Scan t (pure id)

instance Functor (Form t) where

  fmap f (Complete a)   = Complete (f a)
  fmap f (Scan t p)     = Scan t (fmap (f .) p)
  fmap f (Predict r p)  = Predict r (fmap (f .) p)

instance Applicative (Form t) where

  pure = Complete

  Complete f  <*> q = fmap f q
  Scan t p    <*> q = Scan  t (flip <$> p <*> q)
  Predict r p <*> q = Predict r (flip <$> p <*> q)

token :: (info ~ t, Eq t) => t -> Form t t
token x = expect x (== x)

expect :: info -> (t -> Bool) -> Form t t
expect info p = terminal info f
  where
    f t | p t = Just t
    f _       = Nothing

instance (info ~ t, a ~ t, Eq t, IsString t) => IsString (Form t a) where

  fromString = token . fromString
