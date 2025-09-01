module Language.Rosa.Data.ModulePath where

import System.FilePath

type ModulePath = [String]

{-
newtype ModulePath = ModulePath [String]
  deriving (Eq)

instance Show ModulePath where
  show (ModulePath parts) = intercalate "." parts
-}
-- TODO: check if parts contains valid chars
{-
mkModulePath :: String -> Maybe ModulePath
-}

-- TODO: moduleFilePath should return absolute paths
{-
moduleFilePath :: ModulePath -> FilePath
moduleFilePath = addExtension "rosa" . joinPath
-}
