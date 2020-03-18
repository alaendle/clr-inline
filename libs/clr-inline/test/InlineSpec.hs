{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StaticPointers     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS  -Wno-missing-signatures #-}
module InlineSpec (module InlineSpec) where

import Control.Concurrent
import Control.Monad
import Control.Lens
import Clr.Inline
import Clr.Inline.Types.Parse
import Data.Int
import Data.String
import Data.Text as Text (pack)
import Data.Word
import Test.Hspec
import System.Mem
import System.Mem.Weak

[csharp| using System;
         using System.Collections.Generic;
       |]

type DateTime = Clr "System.DateTime"

spec = qqSpec

h_i   = 2 :: Int
h_i16 = 2 :: Int16
h_i32 = 2 :: Int32
h_i64 = 2 :: Int64
h_w16 = 1 :: Word16
h_w32 = 1 :: Word32
h_w64 = 1 :: Word64
h_d = 2.2 :: Double
h_b = False
h_s = "Hello from Haskell"
h_t = Text.pack h_s

topHandler =
    [csharp|
           AppDomain currentDomain = default(AppDomain);
           currentDomain = AppDomain.CurrentDomain;
           currentDomain.UnhandledException += (sender, args) => {
                 Console.WriteLine(((Exception)args.ExceptionObject).GetBaseException().ToString());
                 if(((Exception)args.ExceptionObject).GetBaseException().StackTrace == null)
                     Console.WriteLine("Stack trace is null");
                 };
           |]

qqSpec :: Spec
qqSpec = beforeAll_ (startClr >> topHandler) $ do
  it "C# inlines pick up imports" $
    [csharp| var foo = new Dictionary<int,int>(); return ;|]

  it "C# arrays are handled" $ do
      i_array <- [csharp| int[] {
                        int[] a = new int[4]{0,0,0,0};
                        for(int i=0; i < 4; i++) {
                          a[i] = i;
                        }
                        return a;
                        }|]
      [csharp|int{return ($i_array:int[])[2];}|] `shouldReturn` 2


gcUntil cond = do
  let loop 10 = error "gc: tried too many times"
      loop i  = do
        performGC
        threadDelay (i * 10000)
        success <- cond
        unless success $ loop (i+1)
  loop 0
