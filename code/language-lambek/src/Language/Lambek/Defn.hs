module Language.Lambek.Defn where

import Language.Lambek.Ident

data Defn
  = Data Ident [Pat] [ConDecl]
  | Fun
  | Class Ident [Pat] [Defn]
  | Instance Ident [Pat] [Defn]

data ConDecl
  = ConDecl Ident Type

data Expr
  = App Expr Expr
  | Var Ident
  | Lam Ident Expr
  | Let (Ident, Expr) Expr

type Type = Expr

data Pat
  = PatVar Ident
  