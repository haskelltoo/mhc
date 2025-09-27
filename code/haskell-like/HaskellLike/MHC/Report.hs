module HaskellLike.MHC.Report where

import Prelude
import Prettyprinter (Doc)
import Prettyprinter qualified as Pretty

import HaskellLike.MHC.Located (Span)
import HaskellLike.MHC.Located qualified as Located
import HaskellLike.MHC.Parsec (pos)
import HaskellLike.MHC.Parsec (ParseError, errorPos)

data Report
  = ParseError !Span ParseError
  | Context [(Span, Doc ())] Report

parseError :: ParseError -> Report
parseError parsecError =
  ParseError origin parsecError
  where
    origin = pos $ errorPos parsecError    
