{-# LANGUAGE DataKinds #-}

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

import Frontend.Lambek.Prelude
import Language.Lambek.Name

import Prettyprinter

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

  -- | @123@
  Integer :: !Integer -> Token l

  -- | A keyword.
  Keyword :: !Keyword -> Token 'NonLayout

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
  Integer a       == Integer b        = a == b
  Keyword a       == Keyword b        = a == b
  ParenL          == ParenL           = True
  ParenR          == ParenR           = True
  SpecialWord a   == SpecialWord b    = a == b
  Word a          == Word b           = a == b
  _               == _                = False

instance Pretty (Token l) where
  pretty token = case token of
    ArrowL -> pretty "<-"
    ArrowR -> pretty "->"
    Begin -> pretty ":{"
    BracketL -> pretty "["
    BracketR -> pretty "]"
    Character c -> pretty c
    Colon -> pretty ":"
    Comma -> pretty ","
    End -> pretty ":}"
    Equals -> pretty "="
    Integer x -> pretty x
    Keyword kw -> pretty kw
    SpecialWord kw -> pretty kw
    ParenL -> pretty "("
    ParenR -> pretty ")"
    Word word -> pretty word

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
    Case -> pretty "case"
    Class -> pretty "class"
    Data -> pretty "data"
    Do -> pretty "do"
    In -> pretty "in"
    Instance -> pretty "instance"
    Let -> pretty "let"
    Of -> pretty "of"
    Record -> pretty "record"
    Where -> pretty "where"
