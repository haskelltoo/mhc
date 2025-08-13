{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecursiveDo        #-}

module Main where

import Hanjiru
import Hanjiru.Prelude

import Data.Char

main :: IO ()
main =
  do
    -- let out = parse SlowLR0 expr "A + B * C"
    -- print out
    pure ()

data Expr
  = Var String
  | Add Expr Expr
  | Mul Expr Expr
  deriving Show

expr :: Input [String] String => Knot (Kata String Expr)
expr = mdo
  goal <- def "goal" [sum]

  var <- Main.var

  let add a b = do
        pure $ Add a b

  sum <- def "sum"
    [ Add <$> sum <* token "+" <*> product
    , product
    ]

  let mul a b = do
        pure $ Mul a b

  product <- def "product"
    [ Mul <$> product <* token "*" <*> atom
    , atom
    ]

  atom <- def "atom"
    [ var
    , token "(" *> sum <* token ")"
    ]

  pure goal

var :: Knot (Kata String Expr)
var = def "var"
    [ Var <$> ident
    ]

ident :: Kata String String
ident = expect "ident" f
  where
    f (x:_) = isAlpha x
    f _     = False
    