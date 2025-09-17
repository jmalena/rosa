{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Decl where

import Language.Rosa.Core
import Language.Rosa.Ast.Expr
import Language.Rosa.Ast.Pattern

import qualified Data.List.NonEmpty as NE

type family XUseModule p
type family XTySign    p
type family XPatBind   p
type family XFunBind   p

data Decl p
  -- | Module import, e.g. "use rosa.mem.alloc".
  = UseModule
    { useModuleAnn  :: XUseModule p
    , useModulePath :: ModulePath
    }
    -- | Type signature, e.g. "f : int -> int"
  | TySign
    { tySignAnn :: XTySign p
    , tySignId  :: String
    , tySignRhs :: Expr p
    }
    -- | Pattern bind, e.g. "(lhs_1, _) := rhs".
  | PatBind
    { patBindAnn :: XPatBind p
    , patBindLhs :: Pattern p
    , patBindRhs :: Expr p
    }
    -- | Function bind, e.g. "f x := 2*x".
  | FunBind
    { funBindAnn     :: XFunBind p
    , funBindId      :: String
    , funBindMatches :: NE.NonEmpty (Pattern p)
    , funBindRhs     :: Expr p
    }
