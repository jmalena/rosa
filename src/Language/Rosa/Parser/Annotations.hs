{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Parser.Annotations where

import Language.Rosa.Ast
import Language.Rosa.Core

data ParserPhase

------------------------------------------------------------
-- Declarations
------------------------------------------------------------

type instance XUseModule ParserPhase = SrcSpan
type instance XTySign    ParserPhase = SrcSpan
type instance XPatBind   ParserPhase = SrcSpan
type instance XFunBind   ParserPhase = SrcSpan

------------------------------------------------------------
-- Expression
------------------------------------------------------------

type instance XVar      ParserPhase = SrcSpan
type instance XApp      ParserPhase = SrcSpan
type instance XAbs      ParserPhase = SrcSpan
type instance XPi       ParserPhase = SrcSpan
type instance XUniverse ParserPhase = SrcSpan
type instance XTyped    ParserPhase = SrcSpan
type instance XLet      ParserPhase = SrcSpan
type instance XBoolLit  ParserPhase = SrcSpan
type instance XIntLit   ParserPhase = SrcSpan

instance HasAnn (Expr ParserPhase) where
  type AnnType (Expr ParserPhase) = SrcSpan

  ann (Var a _)      = a
  ann (App a _ _)    = a
  ann (Abs a _ _)    = a
  ann (Pi a _ _ _)   = a
  ann (Universe a _) = a
  ann (Typed a _ _)  = a
  ann (Let a _ _ _)  = a
  ann (BoolLit a _)  = a
  ann (IntLit a _)   = a

  setAnn a' (Var _ x)        = Var a' x
  setAnn a' (App _ f arg)    = App a' f arg
  setAnn a' (Abs _ p e)      = Abs a' p e
  setAnn a' (Pi _ p ty body) = Pi a' p ty body
  setAnn a' (Universe _ l)   = Universe a' l
  setAnn a' (Typed _ e t)    = Typed a' e t
  setAnn a' (Let _ p e body) = Let a' p e body
  setAnn a' (BoolLit _ b)    = BoolLit a' b
  setAnn a' (IntLit _ n)     = IntLit a' n

------------------------------------------------------------
-- Patterns
------------------------------------------------------------

type instance XPCon      ParserPhase = SrcSpan
type instance XPVar      ParserPhase = SrcSpan
type instance XPInt      ParserPhase = SrcSpan
type instance XPWildcard ParserPhase = SrcSpan

instance HasAnn (Pattern ParserPhase) where
  type AnnType (Pattern ParserPhase) = SrcSpan

  ann (PCon a _ _)  = a
  ann (PVar a _)    = a
  ann (PInt a _)    = a
  ann (PWildcard a) = a

  setAnn a' (PCon _ name args) = PCon a' name args
  setAnn a' (PVar _ name)      = PVar a' name
  setAnn a' (PInt _ x)         = PInt a' x
  setAnn a' (PWildcard _)      = PWildcard a'
