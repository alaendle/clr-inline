{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeInType        #-}
module Clr.FSharp.Gen (name, compile) where

import           Clr.Inline.Config
import           Clr.Inline.Quoter
import           Clr.Inline.Utils
import           Clr.Inline.Utils.Embed
import           Clr.Inline.Types
import           Control.Lens hiding ((<.>))
import           Control.Monad
import           Control.Monad.Trans.Writer
import qualified Data.ByteString                 as BS
import           Data.List
import qualified Data.Map as Map
import           Data.Proxy
import           Language.Haskell.TH.Syntax
import           System.Directory
import           System.FilePath                 ((<.>), (</>))
import           System.IO.Temp
import           System.Process
import           Text.Printf

name :: Proxy "fsharp"
name = Proxy

paramNames :: [String]
paramNames = [printf "x%d" x | x <- [0::Int ..] ]

genCode :: ClrInlinedGroup "fsharp" -> String
genCode ClrInlinedGroup {units, mod} =
  unlines $
  execWriter $ do
    yield $ printf "namespace %s" (getNamespace mod)
    forM_ units $ \case
      ClrInlinedDec _ body -> yield body
      ClrInlinedExp {} -> return ()
    yield $ printf "type %s =" (getClassName mod)
    forM_ units $ \case
      ClrInlinedDec {} -> return ()
      ClrInlinedExp exp@ClrInlinedExpDetails {..} -> do
        let argsString =
              case Map.toList args of
                [] -> "()"
                other -> unwords [printf "(%s:%s)" a argType
                                 | (a, argDetails) <- other
                                 , let argType = case argDetails of
                                                   Value (ClrTypeSymbol t) -> t
                                                   Delegate _ []   Nothing -> "System.Action"
                                                   Delegate _ args Nothing -> printf "System.Action<%s>" (intercalate "," (map getClrTypeSymbol args))
                                                   Delegate _ args (Just res) -> printf "System.Func<%s>" (intercalate "," (map getClrTypeSymbol args ++ [getClrTypeSymbol res]))
                                 ]
        yield $ printf "#line %d \"%s\"" (fst $ loc_start loc) (loc_filename loc)
        yield $ printf   "  static member %s %s =" (getMethodName exp) argsString
        iforMOf_ (ifolded<._Delegate) args $ \ argName (_,args,_) -> do
          let params = take (length args) paramNames
              formalParams = case params of [] -> "()" ; aa -> unwords aa
              actualParams = intercalate "," params
          yield $ printf "    let %s %s = %s.Invoke(%s) in" argName formalParams argName actualParams
        yield "     ("
        yield $ printf "#line %d \"%s\"" (fst $ loc_start loc) (loc_filename loc)
        forM_ (lines body) $ \l ->
          yield $ printf "        %s" l
        yield "     )"

compile :: ClrInlineConfig -> ClrInlinedGroup "fsharp" -> IO ClrBytecode
compile ClrInlineConfig {..} m@ClrInlinedGroup {..} = do
  temp <- getTemporaryDirectory
  let ass = getAssemblyName name mod
  dir <- createTempDirectory temp "inline-fsharp"
  let src = dir </> ass <.> ".fs"
      proj = dir </> ass <.> ".fsproj"
      tgt = dir </> ass <.> ".dll"
  writeFile src (genCode m)
  writeFile proj ("<Project Sdk=\"Microsoft.NET.Sdk\"><PropertyGroup><TargetFramework>netstandard2.0</TargetFramework></PropertyGroup><ItemGroup><Compile Include=\"" <> ass <> ".fs\"/></ItemGroup></Project>")
  callCommand $
    unwords $
    execWriter $ do
      yield configFSharpPath
      yield "build"
      yield "--nologo"
      yield $ "--output " <> dir
      when (not configDebugSymbols) $ yield "--configuration Release"
      --forM_ configExtraIncludeDirs $ \dir -> yield $ "--lib:" ++ dir
      --forM_ configDependencies $ \name -> yield $ "--reference:" ++ name
      yieldAll configCustomCompilerFlags
      yield proj
  bcode <- BS.readFile tgt
  return $ ClrBytecode bcode
