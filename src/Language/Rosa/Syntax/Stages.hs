{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | AST annotation definitions for different stages.
--   For reference, see the paper "Trees that Grow".
module Language.Rosa.Syntax.Stages (
  Parsed(..),
  Typed(..),
  XDecl,
  XStmt,
  XScopedStmt,
  XExpr
) where

-- | AST stages
data Parsed
data Typed

type family XDecl p
type family XStmt p
type family XScopedStmt p
type family XExpr p

type instance XDecl Parsed = ()
type instance XDecl Typed  = ()
type instance XStmt Parsed          = ()
type instance XStmt Typed           = ()
type instance XScopedStmt Parsed    = ()
type instance XScopedStmt Typed     = ()
type instance XExpr Parsed          = ()
type instance XExpr Typed           = ()
