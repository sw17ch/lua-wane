module Options
       ( runWithOptions
       , WaneOptions(..)
       ) where

import Options.Applicative

data WaneOptions = WaneOptions
  { infile :: FilePath
  , outfile :: FilePath
  } deriving (Show)


runWithOptions :: (WaneOptions  -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

options :: ParserInfo WaneOptions
options = info (helper <*> optParser)
          ( fullDesc
         <> progDesc "Minify Lua code by removing extra spaces and comments."
         <> header "lua-wane - a Lua source code minifier"
          )

optParser :: Parser WaneOptions
optParser = WaneOptions
  <$> strArgument
    ( metavar "INFILE"
   <> help "Lua input source file"
    )
  <*> strArgument
    ( metavar "OUTFILE"
   <> help "Lua output source file. - for stdout."
    )
