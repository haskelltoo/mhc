module HaskellLike.Inform where

import Prelude
import Prettyprinter qualified as Pretty

import HaskellLike.Located
import HaskellLike.Report

-- | Monads with informative reporting.

class Monad m => MonadInform m where

  -- | Make a report.

  report :: Report -> m ()

  -- | Halt immediately.

  halt :: m a

  -- | Halt if there are any reported errors.

  checkpoint :: m a -> m a

  -- | Add context to reports.

  within :: Span -> Pretty.Doc () -> m a -> m a
