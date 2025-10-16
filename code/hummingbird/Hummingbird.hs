module Hummingbird
  (
  -- * Hummingbird

  -- | I am Hummingbird, a small concatenative programming language.
  --

  -- * Terms
  HbTerm (..),
  HbAlt (..),
  HbBind (..),

  -- * Literals
  HbLiteral (..),

  -- * Types
  HbType (..),

  -- * Declarations
  HbMod (..),
  HbFeather (..),
  ) where

import Foldable
import Monoid
import Prelude
import Prettyprinter qualified as Pretty
import Semigroup
import Traversable

-- | I concatenate!

instance Monoid (HbTerm ty binder) where
  mconcat = catTerms

-- |

data HbTerm ty binder
  = Word binder
  | Lit HbLiteral
  | Lambda binder (HbTerm ty binder)
  | Match [HbAlt ty binder]
  | Quoted (HbTerm ty binder)
  | Concat [HbTerm ty binder]
  deriving (Show)

instance Semigroup (HbTerm ty binder) where
  sconcat = catTerms . toList

-- |

catTerms :: [HbTerm ty binder] -> HbTerm ty binder
catTerms = \case
  [term]  -> term
  terms   -> Concat $ concatMap uncatTerms terms

-- |

uncatTerms :: HbTerm ty binder -> [HbTerm ty binder]
uncatTerms = \case
  Concat xs -> xs
  term      -> [term]

-- |

data HbAlt ty binder
  = LitAlt HbLiteral (HbTerm ty binder)
  deriving (Show)

-- |

data HbBind ty binder = Bind binder (HbTerm ty binder)
  deriving (Show)

instance (Pretty binder, Pretty ty) => Pretty (HbTerm ty binder) where
  pretty = \case
    Word word -> pretty word
    Lit literal -> pretty literal
    Lambda arg body ->
      Pretty.hcat
        [
          Pretty.backslash
        , Pretty.hsep
            [
              pretty arg
            , pretty "->"
            , pretty body
            ]
        ]
    Match alts -> pretty alts
    Quoted quoted ->
      Pretty.brackets $ pretty quoted
    Concat xs ->
      Pretty.hsep $ map pretty xs

instance (Pretty binder, Pretty ty) => Pretty (HbAlt ty binder) where
  pretty = \case
    LitAlt literal term ->
      Pretty.hsep
        [
          pretty literal
        , pretty "->"
        , pretty term
        ]

instance (Pretty binder, Pretty ty) => Pretty (HbBind ty binder) where
  pretty = \case
    Bind name body ->
      Pretty.hsep
        [pretty name, Pretty.equals, pretty body]

-- |

data HbLiteral
  = CharLit   Char
  | IntLit    Integer
  | StringLit Text
  deriving (Show)

instance Pretty HbLiteral where
  pretty = \case
    CharLit char -> pretty char
    IntLit int -> pretty int
    StringLit string -> pretty string

-- |

data HbType ty binder
  = StackTy binder [HbType ty binder]
  | ConTy binder
  | VarTy binder
  | FunTy (HbType ty binder) (HbType ty binder)
  | ConcatTy [HbType ty binder]
  deriving (Show)

instance (Pretty binder, Pretty ty) => Pretty (HbType ty binder) where
  pretty = \case
    StackTy name tys ->
      Pretty.hsep $
        [
          Pretty.hcat
            [
              pretty name
            , pretty ".."
            ]
        ] ++ map pretty tys
    ConTy name -> pretty name
    VarTy name -> pretty name
    FunTy lhs rhs ->
      Pretty.parens $
        Pretty.hsep
          [
            pretty lhs
          , pretty "->"
          , pretty rhs
          ]
    ConcatTy tys ->
      Pretty.hsep $ map pretty tys

-- |

data HbMod ty binder = HbMod binder [HbFeather ty binder]
  deriving (Show)

-- |

data HbFeather ty binder
  = Defn (HbBind ty binder)
  | Sig binder (HbType ty binder)
  deriving (Show)

instance (Pretty binder, Pretty ty) => Pretty (HbMod ty binder) where
  pretty = \case
    HbMod name feathers ->
      Pretty.vcat
        [
          Pretty.hsep
            [
              pretty name
            , pretty "module"
            ]
        , Pretty.vcat $ pretty <$> feathers
        ]

instance (Pretty binder, Pretty ty) => Pretty (HbFeather ty binder) where
  pretty = \case
    Defn bind -> pretty bind
    Sig name ty ->
      Pretty.hsep
        [pretty name, Pretty.colon, pretty ty]

