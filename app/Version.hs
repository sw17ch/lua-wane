{-# LANGUAGE TemplateHaskell #-}

module Version
  ( cabalVersion
  , versionInfo
  , versionString
  ) where

import Development.GitRev
import qualified Paths_lua_wane as C (version)
import Data.Version (showVersion)

cabalVersion :: String
cabalVersion = showVersion C.version

versionInfo :: [(String, String)]
versionInfo =
  [ ("version", cabalVersion)
  , ("git branch", $(gitBranch))
  , ("git commit", $(gitHash))
  , ("git dirty", dirty)
  , ("git commit date", $(gitCommitDate))
  ]
  where
  dirty | $(gitDirty) = "yes"
        | otherwise = "no"

dependencyInfo :: [(String, [(String, String)])]
dependencyInfo = []

versionString :: String
versionString = unlines
  [ k ++ ": " ++ v | (k, v) <- versionInfo]
