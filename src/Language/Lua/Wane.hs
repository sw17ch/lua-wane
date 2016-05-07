module Language.Lua.Wane
    ( minFile
    ) where

import Language.Lua.Parser
import Language.Lua.MinPrinter
import Language.Lua.ShortenNames (shortenNames)

minFile :: Bool -> FilePath -> IO String
minFile keepNames fp = do
  lua <- parseFile fp
  case lua of
    Left err -> error (show err)
    Right ast -> return (show (minprint ast'))
      where ast' = if keepNames then ast else shortenNames ast
