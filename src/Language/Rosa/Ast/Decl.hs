{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Decl where

import qualified Data.List.NonEmpty as NE

import Language.Rosa.Core
import Language.Rosa.Ast.Expr

type family XUseModule p
type family XTySign    p
type family XPatBind   p
type family XFunBind   p

data DeclF p f
  -- | Module import
  = UseModule
    { useModuleAnn  :: XUseModule p
    , useModulePath :: ModulePath
    }
    -- | Type signature
  | TySign
    { tySignAnn :: XTySign p
    , tySignId  :: String
    , tySignRhs :: f
    }
    -- | Pattern binding
  | PatBind
    { patBindAnn :: XPatBind p
    , patBindLhs :: Pattern p
    , patBindRhs :: f
    }
    -- | Function binding
  | FunBind
    { funBindAnn     :: XFunBind p
    , funBindId      :: String
    , funBindMatches :: NE.NonEmpty (Pattern p)
    , funBindRhs     :: f
    }
  deriving Functor

type Decl p = DeclF p (Expr p)
