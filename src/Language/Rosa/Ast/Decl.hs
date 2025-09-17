module Language.Rosa.Ast.Decl where

import Language.Rosa.Core
import Language.Rosa.Ast.Expr
import Language.Rosa.Ast.Pattern

import qualified Data.List.NonEmpty as NE

data Decl
  -- | Module import, e.g. "use rosa.mem.alloc".
  = UseModule
    { useModuleAnn  :: SrcSpan
    , useModulePath :: ModulePath
    }
    -- | Type signature, e.g. "f : int -> int"
  | TySign
    { tySignAnn :: SrcSpan
    , tySignId  :: String
    , tySignRhs :: Expr
    }
    -- | Pattern bind, e.g. "(lhs_1, _) := rhs".
  | PatBind
    { patBindAnn :: SrcSpan
    , patBindLhs :: Pattern
    , patBindRhs :: Expr
    }
    -- | Function bind, e.g. "f x := 2*x".
  | FunBind
    { funBindAnn     :: SrcSpan
    , funBindId      :: String
    , funBindMatches :: NE.NonEmpty Pattern
    , funBindRhs     :: Expr
    }
  deriving (Eq, Show)
