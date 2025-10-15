{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hummingbird.MHC.Rename
  (
  -- * Variables
  Var (..),
  InScope,
  freshen,
  freshenNoShadowing,
  withFresh,
  withFreshNoShadowing,

  -- * Renaming
  renameVar,
  renameTerm,
  renameAlt,
  renameBinds,
  renameLhs,
  renameRhs,
  RenameMessage (..),

  -- * Rename monad
  RenameM,
  runRename,
  ) where

import Map (Map)
import Map qualified
import Monad.Chronicle
import Monad.State
import Monad.Trans
import Prelude
import Prettyprinter qualified as Pretty
import These
import Traversable

import HaskellLike.MHC.Name
import Hummingbird
import Hummingbird.MHC.Parse (Name)

-- |

data Var = Var Name Int
  deriving (Show)

instance Pretty Var where
  pretty = \case
    Var name i ->
      pretty name <> pretty '_' <> pretty i

instance Eq Var where
  Var _ i == Var _ j = i == j

instance Ord Var where
  compare (Var _ i) (Var _ j) = compare i j

-- |

type InScope = Map Name Var

-- |

freshen :: Name -> RenameM Var
freshen name = Var name <$> createFreshId

-- |

withFresh :: Name -> InScope -> (Var -> InScope -> RenameM a) -> RenameM a
withFresh name inScope k = do
  var <- freshen name
  k var (Map.insert name var inScope)

-- |

freshenNoShadowing :: Name -> InScope -> RenameM Var
freshenNoShadowing name inScope =
  case Map.lookup name inScope of
    Nothing ->
      freshen name
    Just other ->
      confess $ Ambiguous name other

-- |

withFreshNoShadowing :: Name -> InScope -> (Var -> InScope -> RenameM a) -> RenameM a
withFreshNoShadowing name inScope k = do
  var <- freshenNoShadowing name inScope
  k var (Map.insert name var inScope)

-- |

renameVar :: InScope -> Name -> RenameM Var
renameVar inScope name =
  case Map.lookup name inScope of
    Just var ->
      pure var
    Nothing ->
      confess $ NotInScope name

-- |

data RenameMessage
  = NotInScope Name
  | Ambiguous Name Var
  | ManyRnMessages [RenameMessage]
  deriving (Show)

instance Semigroup RenameMessage where
  a <> b = catMessages [a, b]

-- |

catMessages :: [RenameMessage] -> RenameMessage
catMessages = \case
  [msg] -> msg
  msgs  -> ManyRnMessages $ concatMap go msgs
  where
    go = \case
      ManyRnMessages xs -> xs
      other             -> [other]

-- |

renameTerm :: InScope -> HbTerm () Name -> RenameM (HbTerm () Var)
renameTerm inScope = \case
  Word name ->
    Word <$> renameVar inScope name

  Lit literal ->
    pure $ Lit literal

  Lambda bndr body ->
    withFresh bndr inScope
      (\bndr' inScope' -> Lambda bndr' <$> renameTerm inScope' body)
  
  Match alts ->
    Match <$> mapM (renameAlt inScope) alts

  Quoted quoted ->
    Quoted <$> renameTerm inScope quoted
  
  Concat terms ->
    Concat <$> mapM (renameTerm inScope) terms

-- |

renameAlt :: InScope -> HbAlt () Name -> RenameM (HbAlt () Var)
renameAlt = error "renameAlt: not yet implemented"

-- |

renameBinds :: InScope -> [HbBind () Name] -> RenameM [HbBind () Var]
renameBinds inScope binds = do
  (inScope', freshened) <- mapAccumM renameLhs inScope binds
  mapM (renameRhs inScope') freshened
    
-- |

renameLhs :: InScope -> HbBind () Name -> RenameM (InScope, (Var, HbTerm () Name))
renameLhs inScope (Bind bndr body) =
  withFreshNoShadowing bndr inScope
    (\bndr' inScope' -> pure (inScope', (bndr', body)))

-- |

renameRhs :: InScope -> (Var, HbTerm () Name) -> RenameM (HbBind () Var)
renameRhs inScope (bndr, body) =
  Bind bndr <$> renameTerm inScope body

createFreshId :: RenameM Int
createFreshId = do
  i <- getFreshId
  modifyFreshId (+ 1)
  pure i

getFreshId :: RenameM Int
getFreshId = freshId <$> getRnState

modifyFreshId :: (Int -> Int) -> RenameM ()
modifyFreshId f = modifyRnState $ \s -> s { freshId = f $ freshId s }

-- |

newtype RenameM a = RenameM
  {
    unRename :: ChronicleT RenameMessage (State RenameState) a
  }
  deriving (Functor, Applicative, Monad)

deriving instance MonadChronicle RenameMessage RenameM

-- |

runRename :: RenameM a -> These RenameMessage a
runRename (RenameM m) = evalState (runChronicleT m) initRnState

data RenameState = RenameState
  {
    freshId :: Int
  }

getRnState :: RenameM RenameState
getRnState = RenameM (lift get)

modifyRnState :: (RenameState -> RenameState) -> RenameM ()
modifyRnState f = RenameM (lift $ modify f)

initRnState :: RenameState
initRnState =
  let
    freshId = 0
  in
    RenameState{..}
