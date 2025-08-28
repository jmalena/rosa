module Language.Rosa.Parser
  ( module Language.Rosa.Parser.Errors
  , module Language.Rosa.Parser.Lexer
  , module Language.Rosa.Parser.Monad
  , module Language.Rosa.Parser.NumericParser
  , module Language.Rosa.Parser.Parser
  , module Language.Rosa.Parser.Token
  ) where

import Language.Rosa.Parser.Errors
import Language.Rosa.Parser.Lexer
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.NumericParser
import Language.Rosa.Parser.Parser
import Language.Rosa.Parser.Token

{-
parseSourceFile :: SourceFile -> Parser Statement
parseSourceFile =
  parseStatement . scanTokens
-}
