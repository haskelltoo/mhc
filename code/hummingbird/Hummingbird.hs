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

import Prelude
import Prettyprinter qualified as Pretty

-- |

data HbTerm ty binder
  = Word binder
  | Lit HbLiteral
  | Lambda binder (HbTerm ty binder)
  | Match [HbAlt ty binder]
  | Quoted (HbTerm ty binder)
  | Concat [HbTerm ty binder]
  deriving (Show)

-- |

data HbAlt ty binder
  = LitAlt HbLiteral (HbTerm ty binder)
  deriving (Show)

-- |

data HbBind ty binder = Bind binder (HbTerm ty binder)
  deriving (Show)

instance (Pretty binder, Pretty ty) => Pretty (HbTerm ty binder) where
  pretty term = case term of
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
  pretty alt = case alt of
    LitAlt literal term ->
      Pretty.hsep
        [
          pretty literal
        , pretty "->"
        , pretty term
        ]

instance (Pretty binder, Pretty ty) => Pretty (HbBind ty binder) where
  pretty bind = case bind of
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
  pretty lit = case lit of
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
  pretty ty = case ty of
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
  pretty hbMod = case hbMod of
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
  pretty feather = case feather of
    Defn bind -> pretty bind
    Sig name ty ->
      Pretty.hsep
        [pretty name, Pretty.colon, pretty ty]

