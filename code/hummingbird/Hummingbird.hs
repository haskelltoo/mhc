module Hummingbird
  (
  -- * Hummingbird
  -- | I am Hummingbird, a small concatenative programming language.

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
  Feather (..),
  ) where

import Prelude
import Prettyprinter qualified as Pretty

-- |

data HbTerm binder ty
  = Word binder
  | Lit HbLiteral
  | Lambda binder (HbTerm binder ty)
  | Match [HbAlt binder ty]
  | Quoted (HbTerm binder ty)
  | Concat [HbTerm binder ty]
  deriving (Show)

-- |

data HbAlt binder ty
  = LitAlt HbLiteral (HbTerm binder ty)
  deriving (Show)

-- |

data HbBind binder ty = Bind binder (HbTerm binder ty)
  deriving (Show)

instance (Pretty binder, Pretty ty) => Pretty (HbTerm binder ty) where
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

instance (Pretty binder, Pretty ty) => Pretty (HbAlt binder ty) where
  pretty alt = case alt of
    LitAlt literal term ->
      Pretty.hsep
        [
          pretty literal
        , pretty "->"
        , pretty term
        ]

instance (Pretty binder, Pretty ty) => Pretty (HbBind binder ty) where
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

data HbType binder ty
  = StackTy binder [HbType binder ty]
  | ConTy binder
  | VarTy binder
  | FunTy (HbType binder ty) (HbType binder ty)
  | ConcatTy [HbType binder ty]
  deriving (Show)

instance (Pretty binder, Pretty ty) => Pretty (HbType binder ty) where
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

data HbMod binder ty = HbMod binder [Feather binder ty]
  deriving (Show)

-- |

data Feather binder ty
  = Defn (HbBind binder ty)
  | Sig binder (HbType binder ty)
  deriving (Show)

instance (Pretty binder, Pretty ty) => Pretty (HbMod binder ty) where
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

instance (Pretty binder, Pretty ty) => Pretty (Feather binder ty) where
  pretty feather = case feather of
    Defn bind -> pretty bind
    Sig name ty ->
      Pretty.hsep
        [pretty name, Pretty.colon, pretty ty]

