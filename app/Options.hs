module Options
       ( runWithOptions
       , WaneOptions(..)
       ) where

import Options.Applicative
import Version

data WaneOptions = WaneOptions
  { infile :: FilePath
  , outfile :: FilePath
  } deriving (Show)


runWithOptions :: (WaneOptions  -> IO ()) -> IO ()
runWithOptions fn = do
  mopts <- execParser options
  case mopts of
    Just opts -> fn opts
    Nothing -> putStr versionString

options :: ParserInfo (Maybe WaneOptions)
options = info (helper <*> o)
          ( fullDesc
         <> progDesc "Minify Lua code by removing extra spaces and comments."
         <> header "lua-wane - a Lua source code minifier"
          )
  where
  o = flag' Nothing (long "version" <> hidden)
      <|> (Just <$> optParser)

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
