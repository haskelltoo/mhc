{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Hummingbird.MHC.Parse where

import Prelude
import Control.Applicative
import Control.Monad

import HaskellLike.MHC.Frontend
import HaskellLike.MHC.Located
import HaskellLike.MHC.Parsec qualified as Parsec
import HaskellLike.MHC.Token qualified as Token

import Hummingbird.MHC
import Hummingbird.MHC.Name

import Hummingbird.MHC.Parsec

featherP :: P Feather
featherP = do
  name <- nameP
  choice
    [
      Defn <$> bindP name
    ]

bindP :: Name -> P HbBind
bindP name =
  Bind name <$ expect Token.Equals <*> exprP

exprP :: P HbExpr
exprP = do
  xs <- some $ wordP <|> lambdaP <|> quotedP
  case xs of
    [x] -> pure x
    _   -> pure $ Concat xs

wordP :: P HbExpr
wordP = Word <$> nameP

lambdaP :: P HbExpr
lambdaP =
  Lambda
    <$ expect Token.Lambda <*> nameP
    <* expect Token.ArrowR <*> exprP

quotedP :: P HbExpr
quotedP = Quoted <$> bracketP exprP

nameP :: P Name
nameP = unqualified

bracketP :: P a -> P a
bracketP =
  between
    (expect Token.BracketL)
    (expect Token.BracketR)
