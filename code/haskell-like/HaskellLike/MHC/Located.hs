{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module HaskellLike.MHC.Located where

import Prelude
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

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
