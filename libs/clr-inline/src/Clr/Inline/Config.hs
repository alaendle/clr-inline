module Clr.Inline.Config where

import Clr.Host.Config

data ClrInlineConfig = ClrInlineConfig
  { configFSharpPath :: FilePath
  , configCSharpPath :: FilePath
  , configDependencies :: [String]
  , configExtraIncludeDirs :: [FilePath]
  , configDebugSymbols :: Bool
  , configCustomCompilerFlags :: [String]
  }

defaultMonoConfig, defaultDotNetConfig, defaultConfig :: ClrInlineConfig
defaultMonoConfig = ClrInlineConfig "dotnet" "dotnet" [] [] False []
defaultDotNetConfig  = ClrInlineConfig "dotnet" "dotnet" [] [] False []
defaultConfig = case defaultHostConfig of
                  ClrHostConfig ClrHostMono -> defaultMonoConfig
                  ClrHostConfig ClrHostDotNet -> defaultDotNetConfig
