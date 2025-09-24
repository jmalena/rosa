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

type instance XUni     ParserPhase = SrcSpan
type instance XVar     ParserPhase = SrcSpan
type instance XImp     ParserPhase = SrcSpan
type instance XAsc     ParserPhase = SrcSpan
type instance XApp     ParserPhase = SrcSpan
type instance XAbs     ParserPhase = SrcSpan
type instance XPi      ParserPhase = SrcSpan
type instance XLet     ParserPhase = SrcSpan
type instance XBoolLit ParserPhase = SrcSpan
type instance XIntLit  ParserPhase = SrcSpan

instance HasAnn (Expr ParserPhase) where
  type AnnType (Expr ParserPhase) = SrcSpan

  ann (Fix (Uni a _))      = a
  ann (Fix (BoolLit a _))  = a
  ann (Fix (IntLit a _))   = a
  ann (Fix (Var a _))      = a
  ann (Fix (Imp a _))      = a
  ann (Fix (Asc a _ _))    = a
  ann (Fix (App a _ _))    = a
  ann (Fix (Abs a _ _))    = a
  ann (Fix (Pi a _ _ _))   = a
  ann (Fix (Let a _ _ _))  = a
  
  setAnn a' (Fix (Uni _ l))        = Fix (Uni a' l)
  setAnn a' (Fix (BoolLit _ b))    = Fix (BoolLit a' b)
  setAnn a' (Fix (IntLit _ n))     = Fix (IntLit a' n)
  setAnn a' (Fix (Var _ x))        = Fix (Var a' x)
  setAnn a' (Fix (Imp _ e))        = Fix (Imp a' e)
  setAnn a' (Fix (Asc _ e ty))     = Fix (Asc a' e ty)
  setAnn a' (Fix (App _ f arg))    = Fix (App a' f arg)
  setAnn a' (Fix (Abs _ p e))      = Fix (Abs a' p e)
  setAnn a' (Fix (Pi _ p ty body)) = Fix (Pi a' p ty body)
  setAnn a' (Fix (Let _ p e body)) = Fix (Let a' p e body)

------------------------------------------------------------
-- Patterns
------------------------------------------------------------

type instance XPWildcard ParserPhase = SrcSpan
type instance XPInt      ParserPhase = SrcSpan
type instance XPCon      ParserPhase = SrcSpan
type instance XPVar      ParserPhase = SrcSpan

instance HasAnn (Pattern ParserPhase) where
  type AnnType (Pattern ParserPhase) = SrcSpan

  ann (Fix (PWildcard a))   = a
  ann (Fix (PInt a _))      = a
  ann (Fix (PCon a _ _))    = a
  ann (Fix (PVar a _))      = a
  ann (Fix (PVarImp a _ _)) = a

  setAnn a' (Fix (PWildcard _))       = Fix (PWildcard a')
  setAnn a' (Fix (PInt _ x))          = Fix (PInt a' x)
  setAnn a' (Fix (PCon _ name args))  = Fix (PCon a' name args)
  setAnn a' (Fix (PVar _ name))       = Fix (PVar a' name)
  setAnn a' (Fix (PVarImp _ name ty)) = Fix (PVarImp a' name ty)
