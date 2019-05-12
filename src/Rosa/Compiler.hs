module Rosa.Compiler (
  compile
) where

import Rosa.CodeGen
import Rosa.Parser

compile :: String -> Either String String
compile s = codegen <$> parse s
