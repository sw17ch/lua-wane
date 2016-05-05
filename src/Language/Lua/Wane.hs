module Language.Lua.Wane
    ( minFile
    ) where

import Language.Lua.Parser
import Language.Lua.MinPrinter

minFile :: FilePath -> IO String
minFile fp = do
  lua <- parseFile fp
  case lua of
    Left err -> error (show err)
    Right ast -> return (show (minprint ast))
