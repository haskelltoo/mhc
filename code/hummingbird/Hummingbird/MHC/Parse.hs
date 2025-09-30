{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Hummingbird.MHC.Parse where

import Prelude
import Control.Applicative
import Control.Monad
import Text.Parsec (Parsec)
import Text.Parsec qualified as Parsec

import HaskellLike.MHC.Frontend
import HaskellLike.MHC.Located
import HaskellLike.MHC.Parsec qualified as Parsec
import HaskellLike.MHC.Token as Token

import Hummingbird.MHC
import Hummingbird.MHC.Name

featherP :: P Feather
featherP = do
  name <- nameP
  Parsec.choice
    [
      Defn . Bind name <$ expect Equals <*> exprP
    ]

exprP :: P HbExpr
exprP =
  foldl' Compose <$> go <*> many go
  where
    go = Parsec.choice
      [
        Push <$> varP
      , Push <$> quotationP
      ]

varP :: P HbVal
varP = Var <$> nameP

quotationP :: P HbVal
quotationP = Quotation <$> brackets exprP

brackets :: P a -> P a
brackets = Parsec.between (expect BracketL) (expect BracketR)

type P = Parsec [Located (Token 'NonLayout)] ()

instance MonadToken (Token 'NonLayout) P where
  tokenLex = Parsec.anyToken
  tokensLex = Parsec.getInput

nameP :: P Name
nameP = tokenPrim go
  where
    go token =
      case unLoc token of
        Token.Word name -> Just name
        _               -> Nothing

expect :: (Show token, Eq token) => token -> Parsec [Located token] u ()
expect token = void $ satisfy ((== token) . unLoc)

satisfy :: (Show token) =>
  (Located token -> Bool) ->
  Parsec [Located token] u (Located token)

satisfy predicate = tokenPrim
  (\token -> if predicate token then Just token else Nothing)

tokenPrim :: (Show token) =>
  (Located token -> Maybe a) ->
  Parsec [Located token] u a

tokenPrim = Parsec.tokenPrim show advance
  where
    advance sourcePos _ tokens =
      case tokens of
        [] -> sourcePos
        At origin _ : _ -> Parsec.beginningOf origin
