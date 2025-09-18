{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module HaskellLike.Located where

import Prelude
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty
import Text.Parsec.Pos

data Located a = At Span a

instance Functor Located where
  fmap f (At origin a) = At origin (f a) 

instance Show a => Show (Located a) where
  show (At _ a) = show a

unLoc :: Located a -> a
unLoc (At origin a) = a

at :: a -> Located b -> Located a
at a (At loc _) = At loc a

atBeginningOf :: a -> Located b -> Located a
atBeginningOf a (At loc _) = At (beginningOf loc) a

atEndOf :: a -> Located b -> Located a
atEndOf a (At loc _) = At (endOf loc) a

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

beginningOf :: Span -> Span
beginningOf span = span
  {
    endLine = span.beginLine
  , endColumn = span.beginColumn
  }

endOf :: Span -> Span
endOf span = span
  {
    beginLine = span.endLine
  , beginColumn = span.endColumn
  }

pos :: SourcePos -> Span
pos = point <$> sourceName <*> sourceLine <*> sourceColumn

parsecBeginning :: Span -> SourcePos
parsecBeginning Span{name, beginLine, beginColumn} =
  newPos (Text.unpack name) beginLine beginColumn
