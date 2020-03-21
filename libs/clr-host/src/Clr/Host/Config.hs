{-# LANGUAGE CPP #-}

module Clr.Host.Config where

data ClrHostType = ClrHostDotNet

data ClrHostConfig = ClrHostConfig ClrHostType

defaultHostConfig = ClrHostConfig ClrHostDotNet

getClrHostConfig :: IO ClrHostConfig
getClrHostConfig = return defaultHostConfig

