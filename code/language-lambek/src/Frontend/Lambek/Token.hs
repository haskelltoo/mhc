{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Lambek.Token
  (
    -- * Tokens
    Token (..)

    -- * Layout
  , Layoutness (..)

    -- * Keywords
  , Keyword (..)
  )
  where

import Prelude
import Prettyprinter

import Language.Lambek.Name

data Token (l :: Layoutness) where

  -- | @<-@
  ArrowL :: Token l

  -- | @->@
  ArrowR :: Token l

  -- | Mark the beginning of a new layout section.
  Begin :: Token 'NonLayout

  -- | @[@
  BracketL :: Token l

  -- | @]@
  BracketR :: Token l

  -- | @'x'@
  Character :: !Char -> Token l

  -- | @:@
  Colon :: Token l

  -- | @,@
  Comma :: Token l

  -- | Mark the end of the current layout section.
  End :: Token 'NonLayout

  -- | @=@
  Equals :: Token l

  -- | An indent.
  Indent :: !Int -> Token 'Layout

  -- | @123@
  Integer :: !Integer -> Token l

  -- | A keyword.
  Keyword :: !Keyword -> Token 'NonLayout

  -- | Lambda, @\\@.
  Lambda :: Token l

  -- | Mark a new line in the current layout section.
  Newline :: Token 'NonLayout

  -- | An operator.
  Operator :: Unqualified -> Token l

  -- | @(@
  ParenL :: Token l

  -- | @)@
  ParenR :: Token l

  -- | A keyword that may affect layout.
  SpecialWord :: !Keyword -> Token 'Layout

  -- | @_@
  Underscore :: Token l

  -- | A user-defined word.
  Word :: Unqualified -> Token l

-- | Whether a token is layout-sensitive.

data Layoutness
  = Layout
  | NonLayout
  deriving (Eq, Show)

instance Eq (Token l) where
  ArrowL          == ArrowL           = True
  ArrowR          == ArrowR           = True
  Begin           == Begin            = True
  BracketL        == BracketL         = True
  BracketR        == BracketR         = True
  Character a     == Character b      = a == b
  Colon           == Colon            = True
  Comma           == Comma            = True
  End             == End              = True
  Equals          == Equals           = True
  Indent a        == Indent b         = a == b
  Integer a       == Integer b        = a == b
  Keyword a       == Keyword b        = a == b
  Lambda          == Lambda           = True
  Newline         == Newline          = True
  Operator a      == Operator b       = a == b
  ParenL          == ParenL           = True
  ParenR          == ParenR           = True
  SpecialWord a   == SpecialWord b    = a == b
  Word a          == Word b           = a == b
  _               == _                = False

instance Pretty (Token l) where
  pretty token = case token of
    ArrowL -> "<-"
    ArrowR -> "->"
    Begin -> ">{"
    BracketL -> lbracket
    BracketR -> rbracket
    Character c -> pretty c
    Colon -> colon
    Comma -> comma
    End -> "}<"
    Equals -> equals
    Indent n -> enclose langle rangle (pretty n)
    Integer x -> pretty x
    Keyword kw -> pretty kw
    Lambda -> backslash
    Newline -> backslash <> "n"
    Operator op -> pretty op
    SpecialWord kw -> pretty kw
    ParenL -> lparen
    ParenR -> rparen
    Word word -> pretty word

instance Show (Token l) where
  showsPrec n = showsPrec n . pretty

data Keyword
  = Case
  | Class
  | Data
  | Do
  | In
  | Instance
  | Let
  | Of
  | Record
  | Where
  deriving (Eq, Ord, Show)

instance Pretty Keyword where
  pretty kw = case kw of
    Case -> "case"
    Class -> "class"
    Data -> "data"
    Do -> "do"
    In -> "in"
    Instance -> "instance"
    Let -> "let"
    Of -> "of"
    Record -> "record"
    Where -> "where"
