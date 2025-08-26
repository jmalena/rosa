{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | AST definitions for Rosa language.
--   Note: Every definition is also a declaration, but not vice versa.
module Language.Rosa.Syntax.AST (
  Component,
  Decl(..),
  BlockStmt,
  ScopedStmt(..),
  Stmt(..),
  Expr(..),
  Ident
) where

import Data.Word (Word64)

import Language.Rosa.Syntax.Operators
import Language.Rosa.Syntax.Stages

type Component p = [Decl p]

-- | Top-level declaration in a component.
data Decl p
  = -- | A named function declaration with a list of parameters.
    --   Example: `int fun();`
    FuncDecl (XDecl p) Ident [Ident]
    -- | A named function definition with a list of parameters
    --   and a compound statement representing the function body.
    --   Example: `int fun() { return 0; }`
  | FuncDefn (XDecl p) Ident [Ident] (BlockStmt p)

-- | Block statement (also known as "compound statement").
--   A list of 'ScopedStmt' enclosed in `{ ... }`.
type BlockStmt p = [ScopedStmt p]

-- | Statements that support local variable declarations.
data ScopedStmt p
  = -- | Local variable declaration.
    --   Example: `int x;`
    VarDecl (XScopedStmt p) Ident
    -- | Local variable definition with an initializer expression.
    --   Example: `int x = 1;`
  | VarDefn (XScopedStmt p) Ident (Expr p)
    -- | A wrapped statement.
  | Stmt (XScopedStmt p) (Stmt p)

-- | Generic statement type.
data Stmt p
  = If (XStmt p) (Expr p) (Stmt p) (Maybe (Stmt p))
  | For (XStmt p) (Maybe (Expr p)) (Maybe (Expr p)) (Maybe (Expr p)) (Stmt p)
  | While (XStmt p) (Expr p) (Stmt p)
  | Do (XStmt p) (Stmt p) (Expr p)
  | Break (XStmt p)
  | Continue (XStmt p)
  | Return (XStmt p) (Expr p)
  -- | A wrapped block statement.
  | Block (XStmt p) (BlockStmt p)
  -- | A wrapped expression.
  | Expr (XStmt p) (Expr p)
  | Noop (XStmt p)

-- | Expressions.
data Expr p
  = NumLit (XExpr p) Word64  -- TODO: Consider using String or Text for arbitrary literals
  | Ref (XExpr p) Ident
  | Assign (XExpr p) Ident (Expr p)
  | FuncCall (XExpr p) Ident [Expr p]
  | UnaryOp (XExpr p) UnaryOp (Expr p)
  | BinaryOp (XExpr p) BinaryOp (Expr p) (Expr p)

-- | Identifier type.
type Ident = String
