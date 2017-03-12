module Clr.Bindings
  ( module Clr.Bindings.Box
  , module Clr.Bindings.Marshal
  , getMethodStub
  , saveDynamicAssembly
  ) where


import Clr.Bindings.Box
import Clr.Bindings.Host
import Clr.Bindings.Marshal
import Clr.Host
import Clr.Marshal
import Data.Word
import Foreign.Ptr


-- | 'saveDynamicAssembly' saves the assembly containing the dynamically-generated
--   wrapper stubs to disk (for debugging purposes).
saveDynamicAssembly :: IO ()
saveDynamicAssembly = unsafeGetPointerToMethod "SaveDynamicAssembly" >>=  makeSaveDynamicAssemblyDelegate

type SaveDynamicAssemblyDelegate = IO ()
foreign import ccall "dynamic" makeSaveDynamicAssemblyDelegate :: FunPtr SaveDynamicAssemblyDelegate -> SaveDynamicAssemblyDelegate




