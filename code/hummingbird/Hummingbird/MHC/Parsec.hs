{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Hummingbird.MHC.Parsec
  (
  P,
  Parsec.anyToken,
  Parsec.between,
  Parsec.choice,
  expect,
  Parsec.located,
  satisfy,
  tokenPrim,
  Parsec.try,
  Parsec.unexpected,
  unqualified,
  ) where

import Applicative
import Monad
import Parsec (Parsec, ParsecT)
import Parsec qualified
import Prelude.GHC

import HaskellLike.MHC.Located
import HaskellLike.MHC.Name
import HaskellLike.MHC.Parsec qualified as Parsec
import HaskellLike.MHC.Token as Token
import HaskellLike.MHC.Tokenize (MonadToken (..))

type P = Parsec [Located (Token 'NonLayout)] ()

instance MonadToken (Token 'NonLayout) P where
  tokenLex = Parsec.anyToken
  tokensLex = Parsec.getInput

unqualified :: P Unqualified
unqualified = tokenPrim go
  where
    go token =
      case unLoc token of
        Word name -> Just name
        _         -> Nothing

expect :: Token 'NonLayout -> P (Located (Token 'NonLayout))
expect token = satisfy ((== token) . unLoc)

satisfy :: (Located (Token 'NonLayout) -> Bool) -> P (Located (Token 'NonLayout))
satisfy predicate = tokenPrim
  (\token -> if predicate token then Just token else Nothing)

tokenPrim :: (Located (Token 'NonLayout) -> Maybe a) -> P a
tokenPrim = Parsec.tokenPrim show advance
  where
    advance sourcePos _ tokens =
      case tokens of
        [] -> sourcePos
        At origin _ : _ -> Parsec.beginningOf origin
