module Language.Lua.Wane
    ( minFile
    ) where

import Language.Lua.Parser
import Language.Lua.MinPrinter
import Language.Lua.ShortenNames (shortenNames)
import Language.Lua.PrettyPrinter (pprint)

minFile :: Bool -> Bool -> FilePath -> IO String
minFile keepNames prettyPrint fp = do
  lua <- parseFile fp
  case lua of
    Left err -> error (show err)
    Right ast ->
      return . show . printIt . workIt $ ast
      where
        printIt = if prettyPrint then pprint else minprint
        workIt  = if keepNames then id else shortenNames
