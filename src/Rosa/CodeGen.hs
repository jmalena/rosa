module Rosa.CodeGen (
  codegen
) where

import Rosa.AST

codegen :: Defn -> String
codegen (Func ident body) = ".global _" ++ ident ++ "\n_" ++ ident ++ ":\n" ++ (mconcat $ codegenStmt <$> body) ++ "  ret\n"

codegenStmt :: Stmt -> String
codegenStmt (Return (Lit (LInt val))) = "  movl $" ++ show val ++ ", %eax\n"
