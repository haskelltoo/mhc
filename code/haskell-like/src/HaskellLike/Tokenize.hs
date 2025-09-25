{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellLike.Tokenize
  (
  -- * Main interface
  MonadToken (..),
  tokenize,
  Tokenize,

  -- * Internal tokenizers
  alphanumeric,
  character,
  colon,
  doubleQuote,
  equals,
  newline,
  operator,
  singleQuote,
  special,
  symbol,
  underscore,
  visible,
  word,
  ) where

import Prelude
import Control.Applicative
import Data.Char
import Data.Functor
import Data.Text (Text)
import Data.Text qualified as Text

import HaskellLike.Inform
import HaskellLike.Located (Located (..))
import HaskellLike.Name
import HaskellLike.Parsec
import HaskellLike.Report qualified as Report
import HaskellLike.Token

-- | Monad capable of lexical analysis on a hidden input string.

class Monad m => MonadToken token m where
  
  -- | Run a continuation on the next token in the input.

  tokenRun :: (Located token -> m a) -> m a
  tokenRun cont =
    tokenLex >>= cont

  {-# INLINE tokenRun #-}

  -- | Get the next token from input.

  tokenLex :: m (Located token)
  tokenLex =
    tokenRun pure

  -- | Convert the whole input into its lexical tokens all at once.

  tokensLex :: m [Located token]

-- | A simple tokenizer monad backed by 'Parsec'.

type Tokenize = Parsec Text ()

instance MonadToken (Token 'Layout) Tokenize where

  tokenLex = choice
    [
      located visible
    , newline
    , space *> tokenLex
    ]

  tokensLex = reverse <$> go []
    where
      go xs = do
        input <- getInput
        if Text.null input then
          pure xs
        else do
          token <- tokenRun pure
          go (token : xs)

-- | Convert a raw text input into a stream of tokens.

tokenize ::
  MonadInform m =>
  Int ->
  FilePath ->
  Text ->
  m [Located (Token 'Layout)]

tokenize line path text =
  case parse tokensLex path text of
    Left errs -> do
      report $ Report.parseError errs
      halt
    Right result -> pure result

{-# INLINE tokenize #-}

-- | Newlines and indentation.

newline :: Tokenize (Located (Token 'Layout))
newline = do
  void endOfLine
  void spaces
  located (Indent <$> sourceColumn <$> getPosition)

-- | Visible tokens.

visible :: Tokenize (Token 'Layout)
visible = choice
  [
    operator
  , word
  , character
  , BracketL <$ char '['
  , BracketR <$ char ']'
  , Comma <$ char ','
  , Lambda <$ char '\\'
  , ParenL <$ char '('
  , ParenR <$ char ')'
  ]

character :: Tokenize (Token 'Layout)
character = between singleQuote singleQuote $ do
  Character <$> choice
    [
      char '\n' *> unexpected ""
    , singleQuote *> unexpected ""
    , char '\\' *> escape
    , anyChar
    ]
  where
    escape = choice
      [
        oneOf "\\\"\'"
      , '\a' <$ char 'a'
      , '\b' <$ char 'b'
      , '\f' <$ char 'f'
      , '\n' <$ char 'n'
      , '\r' <$ char 'r'
      , '\t' <$ char 't'
      , '\v' <$ char 'v'
      , space <* spaces
      ]

colon :: Tokenize (Token 'Layout)
colon = Colon <$ char ':' <* notFollowedBy symbol

equals :: Tokenize (Token 'Layout)
equals = Equals <$ char '=' <* notFollowedBy symbol

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

doubleQuote :: Tokenize Char
doubleQuote = char '\"'

singleQuote :: Tokenize Char
singleQuote = char '\''

special :: Tokenize Char
special = oneOf "\"'(),[\\]_{}"

symbol :: Tokenize Char
symbol = do
  notFollowedBy special
  choice
    [
      satisfy isSymbol
    , satisfy isPunctuation
    ]

underscore :: Tokenize Char
underscore = char '_'
