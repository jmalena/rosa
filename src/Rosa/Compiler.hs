
module Rosa.Compiler (
  compile
) where

import Rosa.Frontend.Parser
import Rosa.Codegen

compile :: String -> Either String String
compile s = (runCodegen . codegen) <$> parse s
