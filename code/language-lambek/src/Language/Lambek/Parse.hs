module Language.Lambek.Parse where

import Language.Lambek.Defn
import Language.Lambek.Ident

import Prelude
import Data.Functor
import Data.Text
import Text.Parsec
import Text.Parsec.Language (haskell)
import Text.Parsec.Token qualified

pDefn :: Parsec String () Defn
pDefn = do
  ident <- pIdent
  pats <- pPatterns
  pKeyword "data" $> Data ident pats <*> pConsDecls

pPatterns :: Parsec String () [Pat]
pPatterns = many pPattern

pPattern :: Parsec String () Pat
pPattern = pure PatVar <*> pIdent

pConsDecls :: Parsec String () [ConsDecl]
pConsDecls =
      (:) <$> pConsDecl <*> pConsDecls
  <|> pure []

pConsDecl :: Parsec String () ConsDecl
pConsDecl = ConsDecl <$> pIdent <*> (pReservedOp "::" *> pExpr)

pExpr :: Parsec String () Expr
pExpr =
      pAExpr
  <|> App <$> pAExpr <*> pAExpr

pAExpr :: Parsec String () Expr
pAExpr = Var <$> pIdent <|> pParens pExpr

data Token
  = TIdent String
  deriving (Eq, Ord, Show)

pIdent :: Parsec String () Ident
pIdent = Ident . pack <$> Text.Parsec.Token.identifier haskell

pParens :: Parsec String () a -> Parsec String () a
pParens = Text.Parsec.Token.parens haskell

pKeyword :: String -> Parsec String () ()
pKeyword = Text.Parsec.Token.reserved haskell

pReservedOp :: String -> Parsec String () ()
pReservedOp = Text.Parsec.Token.reservedOp haskell
