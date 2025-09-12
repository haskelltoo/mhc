{-# LANGUAGE DataKinds #-}

module HaskellLike.Layoutize where

import Prelude
import Control.Applicative
import Text.Parsec (Parsec)
import Text.Parsec qualified as Parsec

import HaskellLike.Inform
import HaskellLike.Report qualified as Report
import HaskellLike.Token

layoutize ::
  MonadInform m =>
  FilePath ->
  [Token 'Layout] ->
  m [Token 'NonLayout]

layoutize path tokens =
  case Parsec.parse layoutMain path tokens
  of
    Left errs -> do
      report $ Report.parseError errs
      halt
    Right result -> pure result

type Layoutize = Parsec [Token 'Layout] ()

layoutMain :: Layoutize [Token 'NonLayout]
layoutMain = error "unimplemented"
