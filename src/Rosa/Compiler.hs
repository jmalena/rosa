module Rosa.Compiler (
  compile
) where

import Rosa.Codegen
import Rosa.Parser

compile :: String -> Either String String
compile s = (runCodegen . codegen) <$> parse s
