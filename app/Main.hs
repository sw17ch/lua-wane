module Main
       ( main
       ) where

import Language.Lua.Wane
import Options

main :: IO ()
main = runWithOptions wane

wane :: WaneOptions -> IO ()
wane (WaneOptions keep inf outf) = do
  minlua <- minFile keep inf
  if "-" == outf
     then putStr minlua
     else writeFile outf minlua
  return ()
