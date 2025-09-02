{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Lambek.Tokenize where

import Prelude
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Data.Functor.Identity
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec (Parsec)
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos qualified as Parsec

import Frontend.Lambek.Token
import Language.Lambek.Name

type Tokenize = Parsec Text ()

tokenize
  :: Int
  -> FilePath
  -> Text
  -> Either Parsec.ParseError [Token 'Layout]

tokenize line path text = Parsec.parse (top line) path text

top :: Int -> Tokenize [Token 'Layout]
top line = do
  pos <- Parsec.getPosition
  Parsec.setPosition (Parsec.setSourceLine pos line)
  many token <* Parsec.eof

token :: Tokenize (Token 'Layout)
token = Parsec.choice
  [
    newline
  , Parsec.space *> token
  , visible
  ]

newline :: Tokenize (Token 'Layout)
newline = do
  void Parsec.endOfLine
  void Parsec.spaces
  Indent <$> Parsec.sourceColumn <$> Parsec.getPosition

visible :: Tokenize (Token 'Layout)
visible = Parsec.choice
  [
    operator
  , word
  , character
  , BracketL <$ Parsec.char '['
  , BracketR <$ Parsec.char ']'
  , Comma <$ Parsec.char ','
  , Lambda <$ Parsec.char '\\'
  , ParenL <$ Parsec.char '('
  , ParenR <$ Parsec.char ')'
  ]

character :: Tokenize (Token 'Layout)
character = Parsec.between singleQuote singleQuote $ do
  Character <$> Parsec.choice
    [
      Parsec.char '\n' *> Parsec.unexpected ""
    , singleQuote *> Parsec.unexpected ""
    , Parsec.char '\\' *> escape
    , Parsec.anyChar
    ]
  where
    escape = Parsec.choice
      [
        Parsec.oneOf "\\\"\'"
      , '\a' <$ Parsec.char 'a'
      , '\b' <$ Parsec.char 'b'
      , '\f' <$ Parsec.char 'f'
      , '\n' <$ Parsec.char 'n'
      , '\r' <$ Parsec.char 'r'
      , '\t' <$ Parsec.char 't'
      , '\v' <$ Parsec.char 'v'
      , Parsec.space <* Parsec.spaces
      ]

colon :: Tokenize (Token 'Layout)
colon = Colon <$ Parsec.char ':' <* Parsec.notFollowedBy symbol

equals :: Tokenize (Token 'Layout)
equals = Equals <$ Parsec.char '=' <* Parsec.notFollowedBy symbol

operator :: Tokenize (Token 'Layout)
operator = do
  str <- Text.pack <$> some symbol
  pure $ case str of
    "<-" -> ArrowL
    "->" -> ArrowR
    ":" -> Colon
    "=" -> Equals
    _ -> Operator (Unqualified str)

word :: Tokenize (Token 'Layout)
word = do
  str <- alphanumeric
  pure $ case str of
    "case" -> SpecialWord Case
    "class" -> SpecialWord Class
    "data" -> SpecialWord Data
    "do" -> SpecialWord Do
    "in" -> SpecialWord In
    "instance" -> SpecialWord Instance
    "let" -> SpecialWord Let
    "of" -> SpecialWord Of
    "record" -> SpecialWord Record
    "where" -> SpecialWord Where
    _ -> Word (Unqualified str)

alphanumeric :: Tokenize Text
alphanumeric = do
  x <- letter <|> underscore
  xs <- many (letter <|> underscore <|> digit)
  pure $ Text.pack $ x : xs

digit :: Tokenize Char
digit = Parsec.digit

letter :: Tokenize Char
letter = Parsec.satisfy isLetter

doubleQuote :: Tokenize Char
doubleQuote = Parsec.char '\"'

singleQuote :: Tokenize Char
singleQuote = Parsec.char '\''

special :: Tokenize Char
special = Parsec.oneOf "\"'(),[\\]_{}"

symbol :: Tokenize Char
symbol = do
  Parsec.notFollowedBy special
  Parsec.choice
    [
      Parsec.satisfy isSymbol
    , Parsec.satisfy isPunctuation
    ]

underscore :: Tokenize Char
underscore = Parsec.char '_'
