{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecursiveDo        #-}

module Main where

import Hanjiru
import Hanjiru.Prelude

import Data.Char
import Data.Maybe

main :: IO ()
main =
  do
    let out = parse (SlowEarley @()) expr $ words "A + B * C"
    print $ take 5 out

data Expr
  = Var String
  | Add Expr Expr
  | Mul Expr Expr
  deriving Show

expr :: Input [String] String => Knot Identity (Kata String Expr)
expr = mdo
  goal <- def "goal" [sum]

  var <- Main.var

  let add a b = do
        pure $ Add a b

  sum <- def "sum"
    [ product
    , Add <$> sum <* token "+" <*> product
    ]

  let mul a b = do
        pure $ Mul a b

  product <- def "product"
    [ atom
    , Mul <$> product <* token "*" <*> atom
    ]

  atom <- def "atom"
    [ var
    , token "(" *> sum <* token ")"
    ]

  pure goal

var :: Knot Identity (Kata String Expr)
var = def "var"
    [ Var <$> ident
    ]

ident :: Kata String String
ident = expect "ident" f
  where
    f (x:_) = isAlpha x
    f _     = False
    