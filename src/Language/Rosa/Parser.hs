module Language.Rosa.Parser
  ( module Language.Rosa.Parser.Lexer
  , module Language.Rosa.Parser.Monad
  , module Language.Rosa.Parser.Parser
  , module Language.Rosa.Parser.Token
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Ast
import Language.Rosa.Parser.Lexer
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.Parser
import Language.Rosa.Parser.Token

parseString :: BL.ByteString -> Parser Expr
parseString = moduleP . tokenize

parseFile :: FilePath -> IO (Parser Expr)
parseFile path = parseString <$> BL.readFile path
