{-# LANGUAGE FlexibleInstances #-}

module Hanjiru.Language
  (
    Kata
  , Move (..)
  
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

-- | 

type Kata t = Ap (Move t)

-- | The Earley parsing algorithm moves.

data Move t a
  = Scan (t -> Maybe a)
  | Predict [Kata t a]

instance Functor (Move t) where

  fmap f (Scan t)     = Scan (fmap f . t)
  fmap f (Predict rs) = Predict (fmap f <$> rs)

infixr 0 <?>

(<?>) :: info -> [Kata t a] -> Knot (Kata t a)
(<?>) = def

def :: info -> [Kata t a] -> Knot (Kata t a)
def info alts = Bend (Ap (Predict alts) $ pure id) Tie

terminal :: info -> (t -> Maybe a) -> Kata t a
terminal info t = Ap (Scan t) (pure id)

token :: (info ~ t, Eq t) => t -> Kata t t
token x = expect x (== x)

expect :: info -> (t -> Bool) -> Kata t t
expect info p = terminal info f
  where
    f t | p t = Just t
    f _       = Nothing

instance (info ~ t, a ~ t, Eq t, IsString t) => IsString (Kata t a) where

  fromString = token . fromString
