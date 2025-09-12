module HaskellLike.Report where

import Prelude
import Prettyprinter qualified as Pretty
import Text.Parsec qualified as Parsec

import HaskellLike.Located (Span)
import HaskellLike.Located qualified as Located

data Report
  = ParseError !Span Parsec.ParseError
  | Context [(Span, Pretty.Doc ())] Report

parseError :: Parsec.ParseError -> Report
parseError parsecError =
  ParseError origin parsecError
  where
    origin = Located.pos $ Parsec.errorPos parsecError    
