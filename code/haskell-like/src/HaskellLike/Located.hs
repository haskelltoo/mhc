{-# LANGUAGE OverloadedRecordDot #-}

module HaskellLike.Located where

import Prelude
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty
import Text.Parsec.Pos

data Located a = At Span a

instance Show a => Show (Located a) where
  show (At _ a) = show a

data Span = Span
  {
    name :: Text
  , beginLine :: !Int
  , beginColumn :: !Int
  , endLine :: !Int
  , endColumn :: !Int
  }
  deriving (Eq, Show)

instance Pretty Span where
  pretty origin = Pretty.hcat
    [ pretty origin.name
    , Pretty.colon
    , pretty origin.beginLine
    , Pretty.comma
    , pretty origin.beginColumn
    , pretty "-"
    , if origin.beginLine == origin.endLine then
        pretty origin.endColumn
      else
        Pretty.hcat
          [ pretty origin.endLine
          , Pretty.comma
          , pretty origin.endColumn
          ]
    ]

range :: SourcePos -> SourcePos -> Span
range begin end = Span
  {
    name = Text.pack (sourceName begin)
  , beginLine = sourceLine begin
  , beginColumn = sourceColumn begin
  , endLine = sourceLine end
  , endColumn = sourceColumn end
  }

point :: SourceName -> Line -> Column -> Span
point name line column = Span
  {
    name = Text.pack name
  , beginLine = line
  , beginColumn = column
  , endLine = line
  , endColumn = column
  }

pos :: SourcePos -> Span
pos = point <$> sourceName <*> sourceLine <*> sourceColumn
