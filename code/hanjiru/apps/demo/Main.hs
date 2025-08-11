{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecursiveDo        #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Hanjiru

import Prelude
import Data.Char

main :: IO ()
main =
  do
    let out = parse SlowLR0 expr "A + B * C"
    print out

data Expr
  = Var String
  | Add Expr Expr
  | Mul Expr Expr
  deriving Show

expr :: Input [String] String => Knot (Def r String String (m Expr))
expr = mdo
  goal <- def "goal" [sum]

  var <- Main.var

  let add a b = do
        pure $ Add a b

  sum <- def "sum"
    [ add <$> sum <* "+" <*> product
    , product
    ]

  let mul a b = do
        pure $ Mul a b

  product <- def "product"
    [ mul <$> product <* "*" <*> atom
    , atom
    ]

  atom <- def "atom"
    [ var
    , "(" *> sum <* ")"
    ]

  pure goal

var :: Knot (Def r String String (m Expr))
var = def "var"
    [ pure . Var <$> ident
    ]

ident :: Def r String String String
ident = expect "ident" f
  where
    f (x:_) = isAlpha x
    f _     = False
    