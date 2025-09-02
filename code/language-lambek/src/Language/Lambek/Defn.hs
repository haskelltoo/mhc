module Language.Lambek.Defn where

import Language.Lambek.Ident
import Language.Lambek.Literal

import Internal.Prelude hiding (Type, (<>))
import Text.PrettyPrint.HughesPJLite

data Module
  = Module Ident [Decl]
  deriving Show

data Decl
  = Data Ident [Pat] [ConDecl]
  | Fun
  | Class Ident [Pat] [Decl]
  | Instance Ident [Pat] [Decl]
  deriving Show

data Bind = Bind Ident Expr
  deriving Show

data ConDecl
  = ConDecl Ident Type
  deriving Show

data Expr
  = App Expr Expr
  | Infix Expr [(Ident, Expr)]
  | Var Ident
  | Lit SLoc Literal
  | Lam [Pat] Expr
  | Let [Bind] Expr
  | Case Expr [Alt]
  deriving Show

type Type = Expr

data Alt = Alt Pat Expr
  deriving Show

data Pat
  = WildcardPat
  | VarPat Ident
  | ConPat Ident [Pat]
  | InfixPat Pat [(Ident, Pat)]
  deriving Show

ppDecl :: Decl -> Doc

ppDecl (Data name pats cons) =
  let
    intro = text "Data" <+> ppIdent name <+> ppPats pats <+> text ":="
  in
    hang intro 4 (ppCons cons)

ppDecl _ = text ""

ppBinds :: [Bind] -> Doc
ppBinds = vcat . map ppBind

ppBind :: Bind -> Doc
ppBind (Bind name rhs) = ppIdent name <+> text "=" <+> ppExpr rhs

ppPats :: [Pat] -> Doc
ppPats = hsep . map ppPat

ppPat :: Pat -> Doc
ppPat = go
  where
    go WildcardPat = text "_"
    go (VarPat name) = ppIdent name
    go (ConPat con pats) = parens $ ppIdent con <+> hsep (map ppPat pats)
    go (InfixPat l []) = ppPat l
    go (InfixPat l rs) =
      let
        f (op, r) = ppIdent op <+> ppPat r
      in
        parens $ ppPat l <+> hsep (map f rs)

ppCons :: [ConDecl] -> Doc
ppCons = vcat . map ppCon

ppCon :: ConDecl -> Doc
ppCon (ConDecl name ty) = ppIdent name <+> text "::" <+> ppExpr ty

ppExpr :: Expr -> Doc
ppExpr = ppExpr' id

data ParensM a 

ppExpr' :: (Doc -> Doc) -> Expr -> Doc

ppExpr' _ (Var name) = ppIdent name

ppExpr' withParens expr@App{} = withParens (ppApp [] expr)
  where
    ppApp :: [Expr] -> Expr -> Doc
    ppApp xs (App f x) = ppApp (x:xs) f
    ppApp xs f = hsep (map (ppExpr' parens) (f:xs))

ppExpr' withParens (Infix l rs) =
  let
    f (op, r) = ppIdent op <+> ppExpr' parens r
  in
    withParens $ ppExpr' parens l <+> hsep (map f rs)

ppExpr' withParens (Lam pats body) =
  withParens $
    text "\\" <> ppPats pats <+> text "->" <+>
    ppExpr' id body

ppExpr' withParens (Let binds body) =
  withParens $ vcat
    [ text "let"
    , nest 2 (ppBinds binds)
    , text "in"
    , nest 2 (ppExpr' id body)
    ]

ppExpr' withParens (Case scrut alts) =
  withParens $
    text "case" <+> ppExpr' id scrut <+> text "of" $+$
      nest 4 (ppAlts alts)

ppAlts :: [Alt] -> Doc
ppAlts = vcat . map ppAlt

ppAlt :: Alt -> Doc
ppAlt (Alt pat rhs) = ppPat pat <+> text "->" <+> ppExpr rhs
