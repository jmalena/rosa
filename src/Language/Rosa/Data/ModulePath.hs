module Language.Rosa.Data.ModulePath where

import System.FilePath

type ModulePath = [String]

parseModulePath :: String -> ModulePath
parseModulePath s = splitDots s
  where
    splitDots "" = []
    splitDots str =
      case break (=='.') str of
        (x, "")   -> [x]
        (x, _:xs) -> x : splitDots xs

-- TODO: moduleFilePath should return absolute paths
{-
moduleFilePath :: ModulePath -> FilePath
moduleFilePath = addExtension "rosa" . joinPath
-}
