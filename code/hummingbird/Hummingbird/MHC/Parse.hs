{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Hummingbird.MHC.Parse where

import Prelude

import HaskellLike.MHC.Frontend
import HaskellLike.MHC.Located
import HaskellLike.MHC.Parsec qualified as Parsec
import HaskellLike.MHC.Token qualified as Token

import Hummingbird
import Hummingbird.MHC.Parsec
import Hummingbird.MHC.Var (
    Unqualified (..),
    Name,
  )

hummingbirdP :: P (HbMod () Name)
hummingbirdP = do
  modName <- nameP
  expect $ Token.Keyword Token.Module
  expect Token.Begin
  HbMod modName <$> many (featherP <* expect Token.Newline)

featherP :: P (HbFeather () Name)
featherP = do
  name <- nameP
  choice
    [
      Defn <$> bindP name
    , Sig name <$> sigP
    ]

bindP :: Name -> P (HbBind () Name)
bindP name =
  Bind name <$ expect Token.Equals <*> someTermsP

someTermsP :: P (HbTerm () Name)
someTermsP = do
  xs <- some termP
  case xs of
    [x] -> pure x
    _   -> pure $ Concat xs

termsP :: P (HbTerm () Name)
termsP = do
  xs <- many termP
  case xs of
    [x] -> pure x
    _   -> pure $ Concat xs

termP :: P (HbTerm () Name)
termP = wordP <|> lambdaP <|> quotedP <|> parenP someTermsP

wordP :: P (HbTerm () Name)
wordP = Word <$> nameP

lambdaP :: P (HbTerm () Name)
lambdaP =
  Lambda
    <$ expect Token.Lambda <*> nameP
    <* expect Token.ArrowR <*> someTermsP

quotedP :: P (HbTerm () Name)
quotedP = Quoted <$> bracketP termsP

sigP :: P (HbType () Name)
sigP = expect Token.Colon *> funTyP

typeP :: P (HbType () Name)
typeP = choice
  [
    VarTy <$> nameP
  , try $ parenP funTyP
  , try $ parenP $ ConcatTy <$> some typeP
  ]

funTyP :: P (HbType () Name)
funTyP =
  FunTy <$> stackTyP <* expect Token.ArrowR <*> stackTyP

stackTyP :: P (HbType () Name)
stackTyP = do
  name <- nameP
  expect (Token.Operator (Unqualified ".."))
  StackTy name <$> many typeP

nameP :: P Name
nameP = unqualified

bracketP :: P a -> P a
bracketP =
  between
    (expect Token.BracketL)
    (expect Token.BracketR)

parenP :: P a -> P a
parenP =
  between
    (expect Token.ParenL)
    (expect Token.ParenR)
