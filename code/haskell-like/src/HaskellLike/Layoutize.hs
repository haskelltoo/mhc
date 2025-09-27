{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module HaskellLike.Layoutize where

import Prelude
import Control.Applicative
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Debug.Trace
import Text.Parsec qualified as Parsec

import HaskellLike.Inform
import HaskellLike.Located (
    Located (At),
    Span,
    at,
    atBeginningOf,
    atEndOf,
    unLoc,
  )
import HaskellLike.Parsec hiding (anyToken)
import HaskellLike.Report qualified as Report
import HaskellLike.Token (
    Token (..),
    Keyword (..),
    Layoutness (..),
  )
import HaskellLike.Token qualified as Token
import HaskellLike.Tokenize (
    MonadToken (..),
  )
import Text.Parsec (lookAhead)

instance MonadToken (Token 'NonLayout) Layoutize where

  tokenLex =
    do
      state <- popState
      case state of
        Zero -> layoutKeyword <|> anyToken <|> newline'
        DoNewLayoutContext -> newLayoutContext
        DoNewline loc -> newline loc
        DoEmptyLayout loc -> pure $ At loc End
    where
      newline' = do
        At loc _ <- located indentation
        pushState (DoNewline loc)
        tokenLex

  tokensLex = reverse <$> go []
    where
      go xs = do
        input <- getInput
        if null input then
          pure xs
        else do
          token <- tokenRun @(Token 'NonLayout) pure
          go (token : xs)

instance MonadToken (Token 'Layout) Layoutize where
  tokenLex = Parsec.anyToken
  tokensLex = getInput

layoutize ::
  MonadInform m =>
  [Keyword] ->
  FilePath ->
  [Located (Token 'Layout)] ->
  m [Located (Token 'NonLayout)]

layoutize kws path tokens =
  case runParser tokensLex (initLayoutState kws) path tokens of
    Left errs -> do
      report $ Report.parseError errs
      halt
    Right result -> pure result

defaultKws :: [Keyword]
defaultKws = [Do, Let, Of, Where]

layoutKeyword :: Layoutize (Located (Token 'NonLayout))
layoutKeyword =
  do
    kws <- getLayoutKeywords
    kw <- tokenPrim (go kws)
    pushState DoNewLayoutContext
    pure kw
  where
    go :: [Keyword] -> Located (Token 'Layout) -> Maybe (Located (Token 'NonLayout))
    go kws (At loc token) =
      case token of
        SpecialWord kw ->
          if kw `elem` kws then
            Just $ At loc (Keyword kw)
          else
            Nothing
        _ -> Nothing

newLayoutContext :: Layoutize (Located (Token 'NonLayout))
newLayoutContext = do
  At loc _ <- located indentation
  ctx <- getContext
  indent <- getIndentLevel
  case ctx of
    n:_ | indent <= n -> do
      pushState (DoEmptyLayout loc)
      pure $ At loc Begin
    _ -> do
      pushContext indent
      pure $ At loc Begin

newline :: Span -> Layoutize (Located (Token 'NonLayout))
newline loc = do
  indent <- compareIndent
  case indent of
    GT -> anyToken
    EQ -> pure (At loc Newline)
    LT -> do
      popContext
      pushState (DoNewline loc)
      pure (At loc End)

compareIndent :: Layoutize Ordering
compareIndent = do
  ctx <- getContext
  case ctx of
    []  -> pure GT
    n:_ -> flip compare n <$> getIndentLevel
  
indentation :: Layoutize ()
indentation =
  do
    At loc n <- located $ tokenPrim go
    setIndentLevel n
  where
    go :: Located (Token 'Layout) -> Maybe Int
    go token =
      case unLoc token of
        Indent n -> Just n
        _        -> Nothing
      
type Layoutize = Parsec [Located (Token 'Layout)] LayoutState

data LayoutState = LayoutState
  {
    control :: NonEmpty LayoutControl
  , context :: [Int]
  , indentLevel :: Int
  , layoutKeywords :: [Keyword]
  }

data LayoutControl
  = Zero
  | DoNewLayoutContext
  | DoNewline Span
  | DoEmptyLayout Span

popState :: Layoutize LayoutControl
popState = do
  LayoutState{control} <- getState
  case NonEmpty.uncons control of
    (x, Nothing) -> pure x
    (x, Just xs) -> do
      modifyState (\state -> state{control = xs})
      pure x

pushState :: LayoutControl -> Layoutize ()
pushState x = modifyState
  (\state -> state{control = x `NonEmpty.cons` control state})

getContext :: Layoutize [Int]
getContext = do
  LayoutState{context} <- getState
  pure context

popContext :: Layoutize ()
popContext = modifyState
  (\state -> state{context = drop 1 $ context state})

pushContext :: Int -> Layoutize ()
pushContext x = modifyState
  (\state -> state{context = x : context state})

getIndentLevel :: Layoutize Int
getIndentLevel = indentLevel <$> getState

setIndentLevel :: Int -> Layoutize ()
setIndentLevel n = modifyState
  (\state -> state{indentLevel = n})

getLayoutKeywords :: Layoutize [Keyword]
getLayoutKeywords = layoutKeywords <$> getState

initLayoutState :: [Keyword] -> LayoutState
initLayoutState layoutKeywords =
  let
    control = Zero NonEmpty.:| []
    context = [1]
    indentLevel = 1
  in
    LayoutState{control, context, indentLevel, layoutKeywords}

anyToken :: Layoutize (Located (Token 'NonLayout))
anyToken = tokenPrim
  (\(At loc token) -> At loc <$> Token.fromLayout token)

satisfy :: (Show token, Monad m) =>
  (Located token -> Bool) ->
  ParsecT [Located token] u m (Located token)

satisfy predicate = tokenPrim
  (\token -> if predicate token then Just token else Nothing)

tokenPrim :: (Show token, Monad m) =>
  (Located token -> Maybe a) ->
  ParsecT [Located token] u m a

tokenPrim = Parsec.tokenPrim show advance
  where
    advance sourcePos _ tokens =
      case tokens of
        [] -> sourcePos
        At origin _ : _ -> beginningOf origin
