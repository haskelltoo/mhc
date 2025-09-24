module HaskellLike.Report where

import Prelude
import Prettyprinter (Doc)
import Prettyprinter qualified as Pretty

import HaskellLike.Located (Span)
import HaskellLike.Located qualified as Located
import HaskellLike.Parsec (pos)
import HaskellLike.Parsec (ParseError, errorPos)

data Report
  = ParseError !Span ParseError
  | Context [(Span, Doc ())] Report

parseError :: ParseError -> Report
parseError parsecError =
  ParseError origin parsecError
  where
    origin = pos $ errorPos parsecError    
