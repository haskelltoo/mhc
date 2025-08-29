{-# LANGUAGE DataKinds #-}

module Frontend.Lambek.Token
  (
    -- * Tokens
    Token (..)

    -- * Keywords
  , Keyword (..)

    -- * Layout annotations
  , Layoutness (..)
  )
  where

import Frontend.Lambek.Prelude
import Language.Lambek.Name

import Prettyprinter

data Token (l :: Layoutness) where

  -- >>> Circumfix tokens <<<

  -- | @[@
  BracketL :: Token l

  -- | @]@
  BracketR :: Token l

  -- | @(@
  ParenL :: Token l

  -- | @)@
  ParenR :: Token l

  -- | Begin a new declaration group.
  BeginDecls :: Token 'NonLayout

  -- | End the current declaration group.
  EndDecls :: Token 'NonLayout

  -- >>> Important symbolic tokens <<<

  -- | @<-@
  ArrowL :: Token l

  -- | @->@
  ArrowR :: Token l

  -- | @:@
  Colon :: Token l

  -- | @,@
  Comma :: Token l

  -- | @=@
  Equals :: Token l

  -- >>> Keywords <<<

  -- | A keyword after layout analysis.
  Keyword :: !Keyword -> Token 'NonLayout

  -- | A keyword that requires layout analysis.
  LayoutKeyword :: !Keyword -> Token 'Layout

  -- >>> User-defined words <<<

  -- | An identifier.
  Word :: Unqualified -> Token l

  -- >>> Literal tokens <<<

  -- | @'x'@
  Character :: !Char -> Token l

  -- | @123@
  Integer :: !Integer -> Token l

instance Eq (Token l) where
  BracketL        == BracketL         = True
  BracketR        == BracketR         = True
  ParenL          == ParenL           = True
  ParenR          == ParenR           = True
  BeginDecls      == BeginDecls       = True
  EndDecls        == EndDecls         = True
  Word a          == Word b           = a == b
  Keyword a       == Keyword b        = a == b
  LayoutKeyword a == LayoutKeyword b  = a == b
  ArrowL          == ArrowL           = True
  ArrowR          == ArrowR           = True
  Colon           == Colon            = True
  Comma           == Comma            = True
  Equals          == Equals           = True
  Character a     == Character b      = a == b
  Integer a       == Integer b        = a == b
  _               == _                = False

instance Pretty (Token l) where
  pretty token = case token of
    BracketL -> pretty "["
    BracketR -> pretty "]"
    ParenL -> pretty "("
    ParenR -> pretty ")"
    BeginDecls -> pretty ":{"
    EndDecls -> pretty ":}"
    Word word -> pretty word
    Keyword kw -> pretty kw
    LayoutKeyword kw -> pretty kw
    ArrowL -> pretty "<-"
    ArrowR -> pretty "->"
    Colon -> pretty ":"
    Comma -> pretty ","
    Equals -> pretty "="
    Character c -> pretty c
    Integer x -> pretty x

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

-- | Whether a token is an input or an output (or both) for layout analysis.

data Layoutness
  = Layout    -- ^ Can be an input.
  | NonLayout -- ^ Can be an output.
  deriving (Eq, Show)
