module Main
       ( main
       ) where

import Language.Lua.Wane
import Options

main :: IO ()
main = runWithOptions wane

wane :: WaneOptions -> IO ()
wane (WaneOptions inf outf) = do
  minlua <- minFile inf
  if "-" == outf
     then putStr minlua
     else writeFile outf minlua
  return ()
