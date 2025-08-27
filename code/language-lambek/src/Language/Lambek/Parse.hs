module Language.Lambek.Parse where

import Language.Lambek.Defn
import Language.Lambek.Ident
import Language.Lambek.Lex
import Language.Lambek.Literal

import Internal.Prelude hiding ((*>), (<*))
import Internal.List
import Control.Applicative hiding ((*>), (<*))
import Control.Monad
import Control.Monad.Fail
import Data.Char
import Data.Functor
import Data.List (intercalate, nub)
import Data.Text (Text)
import Text.ParserComb

type P a = Prsr LexState Token a

infixl 4 *>, <*
(*>) :: Prsr s t a -> Prsr s t b -> Prsr s t b
(*>) = (>>)

-- Slightly faster than the <* from Applicative
(<*) :: Prsr s t a -> Prsr s t b -> Prsr s t a
(<*) = (<<)

parse :: Show a => P a -> FilePath -> String -> Either String a
parse p fn file =
  let
    ts = lexTopLS fn file
  in
    case runPrsr p ts of
      Left lf -> Left $ formatFailed lf
      Right a -> Right a

-- * Declarations

pTopModule :: P [Decl]
pTopModule = pModule <* eof

pModule :: P [Decl]
pModule = pBlock pDecl

pDecl :: P Decl
pDecl = do
  ident <- pDeclIdentSym
  pats <- many pAPat
  Data ident pats <$ pKeyword "data" <*> pConDecls

pDeclIdentSym :: P Ident
pDeclIdentSym = pUIdent <|> pParens pSymOper

pBind :: P Bind
pBind = Bind <$> pLIdentSym <* pSpec '=' <*> pExpr

pConDecls :: P [ConDecl]
pConDecls = pBlock pConDecl

pConDecl :: P ConDecl
pConDecl = ConDecl <$> pUIdentSym <* dcolon <*> pExpr

-- * Expressions

pExpr :: P Expr
pExpr = pExprOp

pExprOp :: P Expr
pExprOp = pOperators (pOper <|> pQArrow) pExprArg

pExprArg :: P Expr
pExprArg = pExprApp <|> pLam <|> pLet <|> pCase

pExprApp :: P Expr
pExprApp = do
  f <- pAExpr
  xs <- many pAExprArg
  pure $ foldl App f xs

pAExprArg :: P Expr
pAExprArg = pAExpr <|> pLam <|> pLet <|> pCase

pLam :: P Expr
pLam = Lam
  <$ pSpec '\\' <*> some pAPat
  <* pSRArrow <*> pExpr

pLet :: P Expr
pLet = Let
  <$ pKeyword "let" <*> pBlock pBind
  <* pKeyword "in" <*> pExpr

pCase :: P Expr
pCase = Case
  <$ pKeyword "case" <*> pExpr
  <* pKeyword "of" <*> pBlock pAlt

pAlt :: P Alt
pAlt = Alt <$> pPat <* pSRArrow <*> pExpr

pAExpr :: P Expr
pAExpr =
      Var <$> pLQIdentSym
  <|> Var <$> pUQIdentSym
  <|> pLit
  <|> pParens pExpr

-- * Patterns

pPats :: P [Pat]
pPats = many pPat

pPat :: P Pat
pPat = pPatOp

pPatOp :: P Pat
pPatOp = pPatOperators pUOper pAPat

pAPat :: P Pat
pAPat =
      pPatVar
  <|> pPatCon

pPatVar :: P Pat
pPatVar = VarPat <$> pLIdentSym

pPatCon :: P Pat
pPatCon = ConPat <$> pLIdentSym <*> many pAPat

-----------------------------------------------------------------------
--
-- Everything from here onwards is taken (with permission) from MicroHs.Parse
--
-----------------------------------------------------------------------

pOperators :: P Ident -> P Expr -> P Expr
pOperators oper one = do
  r <- pOperators' oper one
  pure r

pOperators' :: P Ident -> P Expr -> P Expr
pOperators' oper one = mkInfix <$> one <*> many ((,) <$> oper <*> one)
  where
    mkInfix e [] = e
    mkInfix e xs = Infix e xs

pPatOperators :: P Ident -> P Pat -> P Pat
pPatOperators oper one = do
  r <- pPatOperators' oper one
  pure r

pPatOperators' :: P Ident -> P Pat -> P Pat
pPatOperators' oper one = mkInfix <$> one <*> many ((,) <$> oper <*> one)
  where
    mkInfix e [] = e
    mkInfix e xs = InfixPat e xs

pOper :: P Ident
pOper = pQSymOper <|> (pSpec '`' *> pQIdent <* pSpec '`')

pUOper :: P Ident
pUOper = pUQSymOper <|> (pSpec '`' *> pUQIdent <* pSpec '`')

pQSymOper :: P Ident
pQSymOper = do
  let
    is (TIdent loc qs s) | not (isAlpha_ (head s)) && s `notElem` reservedOps = Just (qualName loc qs s)
    is (TSpec  loc '!') = Just (mkIdentSLoc loc "!")
    is (TSpec  loc '~') = Just (mkIdentSLoc loc "~")
    is _ = Nothing
  satisfyM "QSymOper" is

pSymOper :: P Ident
pSymOper = do
  let
    is (TIdent loc [] s) | not (isAlpha_ (head s)) && s `notElem` reservedOps = Just (mkIdentSLoc loc s)
    is (TSpec  loc '!') = Just (mkIdentSLoc loc "!")
    is (TSpec  loc '~') = Just (mkIdentSLoc loc "~")
    is _ = Nothing
  satisfyM "SymOper" is

pUQSymOper :: P Ident
pUQSymOper = guardM pQSymOper isUOper

isUOper :: Ident -> Bool
isUOper = (== ':') . headIdent

pUSymOper :: P Ident
pUSymOper = guardM pSymOper isUOper

pLQSymOper :: P Ident
pLQSymOper = guardM pQSymOper (not . isUOper)

-- Allow -> as well
pLQSymOperArr :: P Ident
pLQSymOperArr = pLQSymOper <|> pQArrow

-- Parse ->, possibly qualified
pQArrow :: P Ident
pQArrow = do
  let
    is (TIdent loc qs s@"->") = Just (qualName loc qs s)
    is (TIdent loc qs s@"\x2192") = Just (qualName loc qs s)
    is _ = Nothing
  satisfyM "->" is

pLSymOper :: P Ident
pLSymOper = guardM pSymOper (not . isUOper)

reservedOps :: [String]
reservedOps = ["::", "<-", "..", "->", "=>",
               "\x2237", "\x2192", "\x21d2"] -- ::, -> and =>

pUQIdentSym :: P Ident
pUQIdentSym = pUQIdent <|> pParens pUQSymOper

-- Lower case, maybe qualified, identifier or symbol
pLQIdentSym :: P Ident
pLQIdentSym = pLQIdent <|> pParens pLQSymOperArr

-- Lower case, unqualified, identifier or symbol
pLIdentSym :: P Ident
pLIdentSym = pLIdent <|> pParens pLSymOper

pLit :: P Expr
pLit = do
  let
    is (TString loc s) = Just (Lit loc (LStr s))
    is (TChar   loc a) = Just (Lit loc (LChar a))
    is (TInt    loc i) = Just (Lit loc (LInteger i))
    is (TRat    loc d) = Just (Lit loc (LRat d))
    is _ = Nothing
  satisfyM "literal" is

pNumLit :: P Expr
pNumLit = guardM pLit isNum
  where isNum (Lit _ (LInteger _)) = True
        isNum (Lit _ (LRat _)) = True
        isNum _ = False

pString :: P String
pString = satisfyM "string" is
  where
    is (TString _ s) = Just s
    is _ = Nothing

dcolon :: P ()
dcolon = pSymbol "::" <|> pSymbol "\x2237"

pDRArrow :: P ()
pDRArrow = pSymbol "=>" <|> pSymbol "\x21d2"

pSRArrow :: P ()
pSRArrow = pSymbol "->" <|> pSymbol "\x2192"

pSLArrow :: P ()
pSLArrow = pSymbol "<-" <|> pSymbol "\x2190"

pSymbol :: String -> P ()
pSymbol sym = void (satisfy sym is)
  where
    is (TIdent _ [] s) = s == sym
    is _ = False

pSpec :: Char -> P ()
pSpec c = void (satisfy (showToken $ TSpec (SLoc "" 0 0) c) is)
  where
    is (TSpec _ d) = c == d
    is _ = False

pParens :: forall a . P a -> P a
pParens p = pSpec '(' *> p <* pSpec ')'

-- Possibly qualified alphanumeric identifier
pQIdent :: P Ident
pQIdent = do
  let
    is (TIdent loc qs s) | isAlpha_ (head s) = Just (qualName loc qs s)
    is _ = Nothing
  satisfyM "QIdent" is

-- Upper case, unqualified, alphanumeric identifier
pUIdentA :: P Ident
pUIdentA = do
  let
    is (TIdent loc [] s) | isUpper (head s) = Just (mkIdentSLoc loc s)
    is _ = Nothing
  satisfyM "UIdent" is

-- Upper case, unqualified identifier
pUIdent :: P Ident
pUIdent =
      pUIdentA
  <|> pUIdentSpecial

-- Upper case, unqualified, identifier or symbol
pUIdentSym :: P Ident
pUIdentSym = pUIdent <|> pParens pUSymOper

-- Special "identifiers": [] (,) ...
pUIdentSpecial :: P Ident
pUIdentSpecial = do
  loc <- getSLoc
  let
    mk = mkIdentSLoc loc
  (mk . map (const ',') <$> (pSpec '(' *> some (pSpec ',') <* pSpec ')'))
    <|> (mk "[]" <$ (pSpec '[' *> pSpec ']'))  -- Allow [] as a constructor name

-- Upper case, possibly qualified, alphanumeric identifier
pUQIdentA :: P Ident
pUQIdentA = do
  let
    is (TIdent loc qs s) | isUpper (head s) = Just (qualName loc qs s)
    is _ = Nothing
  satisfyM "UQIdent" is

-- Upper case, possibly qualified, identifier
pUQIdent :: P Ident
pUQIdent =
      pUQIdentA
  <|> pUIdentSpecial

-- Lower case, unqualified identifier
pLIdent :: P Ident
pLIdent = do
  let
    is (TIdent loc [] s) | isLower_ (head s) && not (isKeyword s) = Just (mkIdentSLoc loc s)
    is _ = Nothing
  satisfyM "LIdent" is

-- Lower case, possibly qualified identifier
pLQIdent :: P Ident
pLQIdent = do
  let
    is (TIdent loc qs s) | isLower_ (head s) && not (isKeyword s) = Just (qualName loc qs s)
    is _ = Nothing
  satisfyM "LQIdent" is

pKeyword :: String -> P ()
pKeyword kw = void (satisfy kw is)
  where
    is (TIdent _ [] s) = kw == s
    is _ = False

isKeyword :: String -> Bool
isKeyword s = elem s keywords

keywords :: [String]
keywords =
  [ "_primitive"
  , "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "forall"
  , "foreign"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "pattern"
  , "then"
  , "type"
  , "where"
  ]

pBraces :: forall a . P a -> P a
pBraces p =
  do
    pSpec '{'
    as <- p
    pSpec '}'
    pure as
 <|>
  do
    pSpec '<'          -- synthetic '{' (i.e., layout)
    as <- p
    -- If we are at a '>' token (i.e., synthetic '}') then
    -- all is well, if not then there is a parse error and we try
    -- recovering by popping they layout stack.
    -- This implements the Note 5 rule from Section 10.3 in
    -- the Haskell report.
    t <- nextToken
    case t of
      TSpec _ '>' -> pSpec '>'
      _           -> mapTokenState popLayout
    pure as

pBlock :: forall a . P a -> P [a]
pBlock p = pBraces body
  where body = sepBy p (some (pSpec ';')) <* optional (pSpec ';')

guardM :: P a -> (a -> Bool) -> P a
guardM ma p = do
  a <- ma
  guard (p a)
  pure a

getSLoc :: P SLoc
getSLoc = do
  t <- nextToken
  pure (tokensLoc [t])

eof :: P ()
eof = do
  t <- nextToken
  case t of
    TEnd _ -> pure ()
    _      -> Control.Monad.Fail.fail "eof"

formatFailed :: LastFail Token -> String
formatFailed (LastFail _ ts msgs) =
  let
    sloc = tokensLoc ts
  in
    showSLoc sloc ++ ":\n"
      ++ "  found:    " ++ head (map showToken ts ++ ["EOF"]) ++ "\n"
      ++ "  expected: " ++ unwords (nub msgs)

qualName :: SLoc -> [String] -> String -> Ident
qualName loc qs s = mkIdentSLoc loc (intercalate "." (qs ++ [s]))

isAlpha_ :: Char -> Bool
isAlpha_ c = isLower_ c || isUpper c
