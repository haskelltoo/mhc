{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hummingbird.MHC.Rename
  (
  -- * Renaming
  renameVar,
  renameTerm,
  renameAlt,
  renameBinds,
  renameLhs,
  renameRhs,
  RenameMessage (..),

  -- * Creating variables
  freshen,
  freshenNoShadowing,
  withFresh,
  withFreshNoShadowing,

  -- * Rename monad
  RenameM,
  runRename,
  ) where

import Map qualified
import Monad.Chronicle
import Monad.State
import Monad.Trans
import Prelude
import Prettyprinter qualified as Pretty
import Semigroup
import These
import Traversable

import Hummingbird
import Hummingbird.MHC.Var (
    Var (..),
    Name,
    InScope,
    VarMap,
  )
import Hummingbird.MHC.Var qualified as Var

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
  sconcat = catMessages . toList

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

renameBinds :: InScope -> [HbBind () Name] -> RenameM (InScope, [HbBind () Var])
renameBinds inScope binds = do
  (inScope', freshened) <- mapAccumM renameLhs inScope binds
  renamed <- mapM (renameRhs inScope') freshened
  pure (inScope', renamed)
    
-- |

renameLhs :: InScope -> HbBind () Name -> RenameM (InScope, (Var, HbTerm () Name))
renameLhs inScope (Bind bndr body) =
  withFreshNoShadowing bndr inScope
    (\bndr' inScope' -> pure (inScope', (bndr', body)))

-- |

renameRhs :: InScope -> (Var, HbTerm () Name) -> RenameM (HbBind () Var)
renameRhs inScope (bndr, body) =
  Bind bndr <$> renameTerm inScope body

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

newtype RenameM a = RenameM
  {
    unRename :: ChronicleT RenameMessage (State RenameState) a
  }
  deriving (Functor, Applicative, Monad)

deriving instance MonadChronicle RenameMessage RenameM

-- |

runRename :: InScope -> (InScope -> RenameM a) -> These RenameMessage a
runRename inScope k =
  evalState
    (runChronicleT $ unRename $ k inScope)
    (initRnState $ length inScope)

createFreshId :: RenameM Int
createFreshId = do
  i <- getFreshId
  modifyFreshId (+ 1)
  pure i

data RenameState = RenameState
  {
    freshId :: Int
  }

getRnState :: RenameM RenameState
getRnState = RenameM (lift get)

getFreshId :: RenameM Int
getFreshId = freshId <$> getRnState

modifyFreshId :: (Int -> Int) -> RenameM ()
modifyFreshId f = modifyRnState $ \s -> s { freshId = f $ freshId s }

modifyRnState :: (RenameState -> RenameState) -> RenameM ()
modifyRnState f = RenameM (lift $ modify f)

initRnState :: Int -> RenameState
initRnState freshId =
  let
  in
    RenameState{..}
