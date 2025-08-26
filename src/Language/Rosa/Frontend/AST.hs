{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Rosa.Frontend.AST (
  ExternDecl(..),
  ScopedStmt(..),
  Stmt(..),
  Expr(..),
  UnaryOp(..),
  BinaryOp(..),
  Ident,
  precOver,
  Parsed(..),
  Typed(..)
) where

import Data.Char
import Data.List
import Data.Word

--------------------------------------------------------------------------------
-- | AST Phases (for reference see paper "Trees that Grow")

data Parsed
data Typed

type family XExternDecl p
type family XStmt p
type family XScopedStmt p
type family XExpr p

type instance XExternDecl Parsed = ()
type instance XExternDecl Typed  = ()
type instance XStmt Parsed       = ()
type instance XStmt Typed        = ()
type instance XScopedStmt Parsed = ()
type instance XScopedStmt Typed  = ()
type instance XExpr Parsed       = ()
type instance XExpr Typed        = ()

--------------------------------------------------------------------------------
-- | AST
--
-- Note: Every definition is also a declaration, but not every declaration
-- is a definition.

-- | Top-level declaration in a program ("external" to any function body).
-- Every constructor carries phase-specific metadata via 'XExternDecl p'.
data ExternDecl p
  = -- | A named function declaration with list of parameters.
    -- Example: `int fun();`
    FuncDecl (XExternDecl p) Ident [Ident]
    -- | A named function definition with a list of parameters
    -- and a compount statement representing function body.
    -- Example: `int fun() { return 0; }`
  | FuncDefn (XExternDecl p) Ident [Ident] (BlockStmt p)

-- | 'Stmt' supportings local variable declaration.
data ScopedStmt p
  = VarDecl (XScopedStmt p) Ident
    -- | Example: `int x;`
  | VarDefn (XScopedStmt p) Ident (Expr p)
    -- | Example: `int x = 1;`
  | Stmt (XScopedStmt p) (Stmt p)
    -- | A nested statement

-- | Statement.
data Stmt p
  = If (XStmt p) (Expr p) (Stmt p) (Maybe (Stmt p))
  | For (XStmt p) (Maybe (Expr p)) (Maybe (Expr p)) (Maybe (Expr p)) (Stmt p)
  | While (XStmt p) (Expr p) (Stmt p)
  | Do (XStmt p) (Stmt p) (Expr p)
  | Break (XStmt p)
  | Continue (XStmt p)
  | Return (XStmt p) (Expr p)
  | Block (XStmt p) (BlockStmt p)
  | Expr (XStmt p) (Expr p)
  | Noop (XStmt p)

-- | Block statement (also known as "Compound statement") is a list of 'ScopedStmt' enclosed in `{ ... }`.
type BlockStmt p = [ScopedStmt p]

-- | Expressions
data Expr p
  = NumLit (XExpr p) Word64 -- TODO: replace Word64 with String or Text
  | Ref (XExpr p) Ident
  | Assign (XExpr p) Ident (Expr p)
  | FuncCall (XExpr p) Ident [Expr p]
  | UnaryOp (XExpr p) UnaryOp (Expr p)
  | BinaryOp (XExpr p) BinaryOp (Expr p) (Expr p)

data UnaryOp
  = OpBitCompl
  | OpLogCompl
  | OpAddCompl

data BinaryOp
  = OpMul
  | OpDiv
  | OpAdd
  | OpSub
  | OpLTE
  | OpLT
  | OpGTE
  | OpGT
  | OpEQ
  | OpNEQ
  | OpLogAnd
  | OpLogOr

type Ident = String

--------------------------------------------------------------------------------
-- Precedence

class Precedence a where
  prec :: a -> Int

instance Precedence UnaryOp where
  prec OpBitCompl = 2
  prec OpLogCompl = 2
  prec OpAddCompl = 2

instance Precedence BinaryOp where
  prec OpMul = 3
  prec OpDiv = 3
  prec OpAdd = 4
  prec OpSub = 4
  prec OpLTE = 6
  prec OpLT = 6
  prec OpGTE = 6
  prec OpGT = 6
  prec OpEQ = 7
  prec OpNEQ = 7
  prec OpLogAnd = 11
  prec OpLogOr = 12

precOver :: (Precedence a, Precedence b)
         => a
         -> b
         -> Bool
precOver a b = prec a > prec b
