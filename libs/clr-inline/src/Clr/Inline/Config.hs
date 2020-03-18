module Clr.Inline.Config where

import Clr.Host.Config

data ClrInlineConfig = ClrInlineConfig
  { configCSharpPath :: FilePath
  , configDependencies :: [String]
  , configExtraIncludeDirs :: [FilePath]
  , configDebugSymbols :: Bool
  , configCustomCompilerFlags :: [String]
  }

defaultMonoConfig, defaultDotNetConfig, defaultConfig :: ClrInlineConfig
defaultMonoConfig = ClrInlineConfig "mcs" [] [] False []
defaultDotNetConfig  = ClrInlineConfig "csc" [] [] False []
defaultConfig = case defaultHostConfig of
                  ClrHostConfig ClrHostMono -> defaultMonoConfig
                  ClrHostConfig ClrHostDotNet -> defaultDotNetConfig
