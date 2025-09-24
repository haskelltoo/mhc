{-# LANGUAGE RecordWildCards #-}

module HaskellLike.Parsec
  (
  -- * Main Parsec interface
  parse,
  Parsec,
  runParserT,
  ParsecT,

  -- * Combinators
  anyChar,
  between,
  char,
  choice,
  digit,
  endOfLine,
  eof,
  letter,
  notFollowedBy,
  oneOf,
  satisfy,
  space,
  spaces,
  try,
  unexpected,

  -- * Location
  located,
  point,
  pos,
  range,
  -- ** Internal to Parsec
  Column,
  Line,
  SourceName,
  SourcePos,
  getPosition,
  setPosition,
  setSourceColumn,
  setSourceLine,
  setSourceName,
  sourceColumn,
  sourceLine,
  sourceName,

  -- * State
  getInput,
  getState,
  modifyState,
  putState,

  -- * Errors
  ParseError,
  errorPos,

  -- * Parsec input stream
  Stream,
  ) where

import Prelude
import Control.Applicative
import Control.Monad
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec
import Text.Parsec.Pos

import HaskellLike.Located (Located (At), Span(..))
import HaskellLike.Located qualified

located :: Monad m => ParsecT s u m a -> ParsecT s u m (Located a)
located p = do
  begin <- getPosition
  a <- p
  end <- getPosition
  pure $ At (range begin end) a

point :: SourceName -> Line -> Column -> Span
point name line column =
  let
    beginLine = line
    beginColumn = column
    endLine = line
    endColumn = column
  in
    Span {name = Text.pack name, ..}

pos :: SourcePos -> Span
pos = point <$> sourceName <*> sourceLine <*> sourceColumn

range :: SourcePos -> SourcePos -> Span
range begin end =
  let
    beginLine = sourceLine begin
    beginColumn = sourceColumn begin
    endLine = sourceLine end
    endColumn = sourceColumn end
  in
    Span{name = Text.pack (sourceName begin), ..}

fromLocated :: Located a -> (a, SourcePos, SourcePos)
fromLocated (At Span{..} a) =
  let
    begin = newPos (Text.unpack name) beginLine beginColumn
    end = newPos (Text.unpack name) endLine endColumn
  in
    (a, begin, end)
