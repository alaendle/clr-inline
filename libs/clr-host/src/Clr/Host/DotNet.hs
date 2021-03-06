{-# LANGUAGE CPP, LambdaCase #-}

module Clr.Host.DotNet (
  startHostDotNet,
  stopHostDotNet
  ) where

import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.String
import Foreign.Storable
import System.Win32
import System.IO
import Control.Exception (bracket)
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as S
import Text.Printf

import Clr.Host.BStr.DotNet
import Clr.Host.BStr.Type
import Clr.Host.DotNet.Common
import Clr.Host.DotNet.Guid
import Clr.Host.DotNet.SafeArray
import Clr.Host.DotNet.Variant
import Clr.Host.Driver

type ICorRuntimeHost = InterfacePtr
type ICLRRuntimeHost = InterfacePtr
type ICLRMetaHost    = InterfacePtr
type ICLRRuntimeInfo = InterfacePtr
type IEnumUnknown    = InterfacePtr

foreign import ccall "dotNetHost.c getICorRuntimeHost" getICorRuntimeHost :: IO ICorRuntimeHost
foreign import ccall "dotNetHost.c getICLRRuntimeHost" getICLRRuntimeHost :: IO ICLRRuntimeHost
foreign import ccall "dotNetHost.c setHostRefs"        setHostRefs        :: ICorRuntimeHost -> ICLRRuntimeHost -> IO ()

#if x86_64_HOST_ARCH
-- 1. binding to __unregister_hs_exception_handler doesn't work reliably e.g. in ghci
-- 2. Calling unregister_hs_exception_handler crashes if no exception handler has been registered previously.
--    This is a problem in 8.2 where it appears the handler is not installed at startup and "main = startClr" crashes.
-- foreign import ccall __unregister_hs_exception_handler :: IO ()
unregister_hs_exception_handler = return () -- __unregister_hs_exception_handler
#else
-- symbol has extra leading underscore on 32 bit apprently
--foreign import ccall ___unregister_hs_exception_handler :: IO ()
--unregister_hs_exception_handler = ___unregister_hs_exception_handler

-- Infact just leave as NO-OP until a 32 bit dev want to investigate the further linker issues
unregister_hs_exception_handler :: IO ()
unregister_hs_exception_handler = return ()
#endif

-- | 'start_ICorRuntimeHost' calls the Start method of the given ICorRuntimeHost interface.
start_ICorRuntimeHost :: ICorRuntimeHost -> IO ()
start_ICorRuntimeHost corHost = do
  -- Initialise COM (and the threading model)
  coInitializeEx nullPtr coInit_ApartmentThreaded
  -- TODO: Allow the library user to select their desired threading model
  --       (we use an STA for the time being so we can use GUI libraries).
  f <- getInterfaceFunction 10 makeStartCor corHost
  f corHost >>= checkHR "ICorRuntimeHost.Start"
  return ()

type Start_ICorRuntimeHost = ICorRuntimeHost -> IO HResult
foreign import stdcall "dynamic" makeStartCor :: FunPtr Start_ICorRuntimeHost -> Start_ICorRuntimeHost

-- | 'start_ICLRRuntimeHost' calls the Start method of the given ICLRRuntimeHost interface.
start_ICLRRuntimeHost :: ICLRRuntimeHost -> IO ()
start_ICLRRuntimeHost clrHost = do
  f <- getInterfaceFunction 3 makeStartCLR clrHost
  f clrHost >>= checkHR "ICLRRuntimeHost.Start"
  return ()

type Start_ICLRRuntimeHost = ICLRRuntimeHost -> IO HResult
foreign import stdcall "dynamic" makeStartCLR :: FunPtr Start_ICLRRuntimeHost -> Start_ICLRRuntimeHost

startHostDotNet :: IO (FunPtr (Ptr Word16 -> IO (FunPtr a)))
startHostDotNet = do
  -- Disable the top level VEH. This can interfere with .NET exceptions.
  unregister_hs_exception_handler
  -- Load the 'mscoree' dynamic library into the process.  This is the
  -- 'stub' library for the .NET execution engine, and is used to load an
  -- appropriate version of the real runtime via a call to
  -- 'CorBindToRuntimeEx'.
  hMscoree <- loadLibrary "mscoree.dll"
  -- Attempt .Net 4.0 binding by first obtaining a pointer to 'CLRCreateInstance'
  -- If this fails, fall back to corBindToRuntimeEx which only allows
  -- binding to MS .Net versions < 4.0
  metaHost <- createMetaHost hMscoree
  if metaHost == nullPtr then do
    corHost <- corBindToRuntimeEx hMscoree
    start_ICorRuntimeHost corHost
    setHostRefs corHost nullPtr
  else getRuntimes_ICLRMetaHost metaHost >>= \case
    []           -> error "No runtime versions found"
    (runtime:xs) -> do
      version <- getVersionString_ICLRRuntimeInfo runtime
      --putStrLn $ "attempting to bind to runtime version " ++ version
      clrHost <- getCLRHost_ICLRRuntimeInfo runtime
      start_ICLRRuntimeHost clrHost
      corHost <- getCorHost_ICLRRuntimeInfo runtime
      setHostRefs corHost clrHost
  corHost <- getICorRuntimeHost
  loadDriverAndBoot corHost

-- | 'stop_ICorRuntimeHost' calls the Stop method of the given ICorRuntimeHost interface.
stop_ICorRuntimeHost :: ICorRuntimeHost -> IO ()
stop_ICorRuntimeHost corHost = do
  f <- getInterfaceFunction 11 makeStop_ICorRuntimeHost corHost
  f corHost >>= checkHR "ICorRuntimeHost.Stop"
  return ()

type Stop_ICorRuntimeHost = ICorRuntimeHost -> IO HResult
foreign import stdcall "dynamic" makeStop_ICorRuntimeHost :: FunPtr Stop_ICorRuntimeHost -> Stop_ICorRuntimeHost

-- | 'stop_ICLRRuntimeHost' calls the Stop method of the given ICLRRuntimeHost interface.
stop_ICLRRuntimeHost :: ICLRRuntimeHost -> IO ()
stop_ICLRRuntimeHost clrHost = do
  f <- getInterfaceFunction 4 makeStop_ICLRRuntimeHost clrHost
  f clrHost >>= checkHR "ICLRRuntimeHost.Stop"
  return ()

type Stop_ICLRRuntimeHost = ICLRRuntimeHost -> IO HResult
foreign import stdcall "dynamic" makeStop_ICLRRuntimeHost :: FunPtr Stop_ICLRRuntimeHost -> Stop_ICLRRuntimeHost

stopHostDotNet :: IO ()
stopHostDotNet = do
  clrHost <- getICLRRuntimeHost
  corHost <- getICorRuntimeHost
  if clrHost == nullPtr then
    if corHost == nullPtr then
      return ()
    else
      stop_ICorRuntimeHost corHost
  else
    stop_ICLRRuntimeHost clrHost
  return ()

clsid_CorRuntimeHost = Guid 0xCB2F6723 0xAB3A 0x11D2 0x9C 0x40 0x00 0xC0 0x4F 0xA3 0x0A 0x3E
iid_ICorRuntimeHost  = Guid 0xCB2F6722 0xAB3A 0x11D2 0x9C 0x40 0x00 0xC0 0x4F 0xA3 0x0A 0x3E

clsid_CLRRuntimeHost = Guid 0x90F1A06E 0x7712 0x4762 0x86 0xB5 0x7A 0x5E 0xBA 0x6B 0xDB 0x02
iid_ICLRRuntimeHost  = Guid 0x90F1A06C 0x7712 0x4762 0x86 0xB5 0x7A 0x5E 0xBA 0x6B 0xDB 0x02

-- | 'corBindToRunTimeEx' loads the CLR execution engine into the process and returns
--   a COM interface for it.
corBindToRuntimeEx :: HINSTANCE -> IO ICorRuntimeHost
corBindToRuntimeEx hMscoree = do
  -- Obtain a pointer to the 'CorBindToRuntimeEx' function from mscoree.dll
  corBindToRuntimeExAddr <- getProcAddress hMscoree "CorBindToRuntimeEx"
  let corBindToRuntimeEx = makeCorBindToRuntimeEx $ castPtrToFunPtr corBindToRuntimeExAddr

  -- Request the shim (mscoree.dll) to load a version of the runtime into the
  -- process, returning a pointer to an implementation of the ICorRuntimeHost
  -- for controlling the runtime.
  with (nullPtr :: ICorRuntimeHost) $ \clrHostPtr -> do
    -- Call 'corBindToRuntimeEx' to obtain an ICorRuntimeHost
    with clsid_CorRuntimeHost $ \refCLSID_CorRuntimeHost ->
      with iid_ICorRuntimeHost $ \refIID_ICorRuntimeHost ->
        corBindToRuntimeEx nullPtr nullPtr 0 refCLSID_CorRuntimeHost
          refIID_ICorRuntimeHost clrHostPtr >>= checkHR "CorBindToRuntimeEx"
    peek clrHostPtr

type CorBindToRuntimeEx = LPCWSTR -> LPCWSTR -> DWORD -> Ptr CLSID -> Ptr IID -> Ptr ICorRuntimeHost -> IO HResult
foreign import stdcall "dynamic" makeCorBindToRuntimeEx :: FunPtr CorBindToRuntimeEx -> CorBindToRuntimeEx

clrCreateInstance :: HINSTANCE -> CLSID -> IID -> IO InterfacePtr
clrCreateInstance hMscoree clsid iid = do
  clrCreateInstanceAddr <- getProcAddress hMscoree "CLRCreateInstance"
  if clrCreateInstanceAddr == nullPtr then
    return nullPtr
  else do
    let clrCreateInstanceRaw = makeCLRCreateInstance $ castPtrToFunPtr clrCreateInstanceAddr
    with (nullPtr :: InterfacePtr) $ \interfacePtr ->
      with clsid $ \refCLSID -> with iid $ \refIID -> do
        hr <- clrCreateInstanceRaw refCLSID refIID interfacePtr
        if hr == 0 then
          peek interfacePtr
        else
          return nullPtr

type CLRCreateInstance = Ptr CLSID -> Ptr IID -> Ptr InterfacePtr -> IO HResult
foreign import stdcall "dynamic" makeCLRCreateInstance :: FunPtr CLRCreateInstance -> CLRCreateInstance

-- | 'createMetaHost'
createMetaHost :: HINSTANCE -> IO ICLRMetaHost
createMetaHost hMscoree = clrCreateInstance hMscoree clsid_CLRMetaHost iid_ICLRMetaHost
                            where clsid_CLRMetaHost = Guid 0X9280188D 0X0E8E 0X4867 0XB3 0X0C 0X7F 0XA8 0X38 0X84 0XE8 0XDE
                                  iid_ICLRMetaHost  = Guid 0XD332DB9E 0XB9B3 0X4125 0X82 0X07 0XA1 0X48 0X84 0XF5 0X32 0X16

-- | 'enumInstalledRuntimes_ICLRMetaHost'
enumInstalledRuntimes_ICLRMetaHost :: ICLRMetaHost -> IO IEnumUnknown
enumInstalledRuntimes_ICLRMetaHost this = do
  f <- getInterfaceFunction 5 makeEnumInstalledRuntimes_ICLRMetaHost this
  with (nullPtr :: IEnumUnknown) $ \enumUnknownPtr -> do
    f this enumUnknownPtr >>= checkHR "EnumerateInstalledRuntimes_ICLRMetaHost"
    peek enumUnknownPtr

type EnumInstalledRuntimes_ICLRMetaHost = ICLRMetaHost -> Ptr IEnumUnknown -> IO HResult
foreign import stdcall "dynamic" makeEnumInstalledRuntimes_ICLRMetaHost :: FunPtr EnumInstalledRuntimes_ICLRMetaHost -> EnumInstalledRuntimes_ICLRMetaHost

-- | 'next_IEnumUnknown'
next_IEnumUnknown :: IEnumUnknown -> IO InterfacePtr
next_IEnumUnknown this = do
  f <- getInterfaceFunction 3 makeNext_IEnumUnknown this
  with (0 :: Word32) $ \fetched ->
    with (nullPtr :: InterfacePtr) $ \interfacePtr -> do
      hr <- f this 1 interfacePtr fetched
      if hr == 0 then
        peek interfacePtr
      else
        return nullPtr

type Next_IEnumUnknown = IEnumUnknown -> Word32 ->  Ptr InterfacePtr -> Ptr Word32 -> IO HResult
foreign import stdcall "dynamic" makeNext_IEnumUnknown :: FunPtr Next_IEnumUnknown -> Next_IEnumUnknown

-- | 'all_IEnumUnknown'
all_IEnumUnknown :: IEnumUnknown -> [InterfacePtr] -> IO [InterfacePtr]
all_IEnumUnknown this xs = do
  next <- next_IEnumUnknown this
  if next == nullPtr then
    return xs
  else
    all_IEnumUnknown this (next : xs)

-- | 'getRuntimes_ICLRMetaHost'
getRuntimes_ICLRMetaHost :: ICLRMetaHost -> IO [ICLRRuntimeInfo]
getRuntimes_ICLRMetaHost this = do
  enum <- enumInstalledRuntimes_ICLRMetaHost this
  all_IEnumUnknown enum []

-- | 'getVersionString_ICLRRuntimeInfo'
getVersionString_ICLRRuntimeInfo :: ICLRRuntimeInfo -> IO String
getVersionString_ICLRRuntimeInfo this = do
  f <- getInterfaceFunction 3 makeGetVersionString_ICLRRuntimeInfo this
  with 512 $ \sizePtr ->
    allocaArray 512 $ \stringPtr -> do
      f this stringPtr sizePtr
      peekCWString stringPtr

type GetVersionString_ICLRRuntimeInfo = ICLRRuntimeInfo -> LPCWSTR -> Ptr DWORD -> IO HResult
foreign import stdcall "dynamic" makeGetVersionString_ICLRRuntimeInfo :: FunPtr GetVersionString_ICLRRuntimeInfo -> GetVersionString_ICLRRuntimeInfo

-- | 'getCorHost_ICLRRuntimeInfo'
getCorHost_ICLRRuntimeInfo :: ICLRRuntimeInfo -> IO ICorRuntimeHost
getCorHost_ICLRRuntimeInfo this = getInterface_ICLRRuntimeInfo this clsid_CorRuntimeHost iid_ICorRuntimeHost

-- | 'getCLRHost_ICLRRuntimeInfo'
getCLRHost_ICLRRuntimeInfo :: ICLRRuntimeInfo -> IO ICLRRuntimeHost
getCLRHost_ICLRRuntimeInfo this = getInterface_ICLRRuntimeInfo this clsid_CLRRuntimeHost iid_ICLRRuntimeHost

-- | 'getInterface_ICLRRuntimeInfo'
getInterface_ICLRRuntimeInfo :: ICLRRuntimeInfo -> Guid -> Guid -> IO InterfacePtr
getInterface_ICLRRuntimeInfo this clsid iid = do
  f <- getInterfaceFunction 9 makeGetInterface_ICLRRuntimeInfo this
  with (nullPtr :: InterfacePtr) $ \interfacePtr -> do
    with clsid $ \refCLSID ->
      with iid $ \refIID ->
        f this refCLSID refIID interfacePtr >>= checkHR "GetInterface_ICLRRuntimeInfo"
    peek interfacePtr

type GetInterface_ICLRRuntimeInfo = ICLRRuntimeInfo -> Ptr CLSID -> Ptr IID -> Ptr ICorRuntimeHost -> IO HResult
foreign import stdcall "dynamic" makeGetInterface_ICLRRuntimeInfo :: FunPtr GetInterface_ICLRRuntimeInfo -> GetInterface_ICLRRuntimeInfo

-- | 'getDefaultDomain_ICorRuntimeHost' calls the GetDefaultDOmain method of the given
--   ICorRuntimeHost interface.
getDefaultDomain_ICorRuntimeHost this = do
  f <- getInterfaceFunction 13 makeGetDefaultDomain this
  with (nullPtr :: InterfacePtr) $ \appDomainPtr -> do
    f this appDomainPtr >>= checkHR "ICorRuntimeHost.GetDefaultDomain"
    peek appDomainPtr

type GetDefaultDomain = ICorRuntimeHost -> Ptr InterfacePtr -> IO HResult
foreign import stdcall "dynamic" makeGetDefaultDomain :: FunPtr GetDefaultDomain -> GetDefaultDomain

type AppDomain = InterfacePtr -- mscorlib::_AppDomain

-- | 'load_AppDomain' calls mscorlib::_AppDomain.Load_3(SafeArray* rawAssembly, _Assembly** result).
load_AppDomain this rawAssembly = do
  f <- getInterfaceFunction 45 makeLoad_AppDomain this
  with (nullPtr :: Assembly) $ \assemblyPtr -> do
    f this rawAssembly assemblyPtr >>= checkHR "AppDomain.Load"
    peek assemblyPtr

type Load_AppDomain = AppDomain -> SafeArray -> Ptr Assembly -> IO HResult
foreign import stdcall "dynamic" makeLoad_AppDomain :: FunPtr Load_AppDomain -> Load_AppDomain

-- | 'load_AppDomain_2' calls mscorlib::_AppDomain.Load_2(BStr assemblyString, _Assembly** result).
load_AppDomain_2 this assemblyString = do
  f <- getInterfaceFunction 44 makeLoad_AppDomain_2 this
  withBStr assemblyString $ \assemblyString' -> do
    with (nullPtr :: InterfacePtr) $ \assemblyPtr -> do
      f this assemblyString' assemblyPtr >>= checkHR "AppDomain.Load"
      peek assemblyPtr

type Load_AppDomain_2 = AppDomain -> BStr -> Ptr InterfacePtr -> IO HResult
foreign import stdcall "dynamic" makeLoad_AppDomain_2 :: FunPtr Load_AppDomain_2 -> Load_AppDomain_2

type Assembly = InterfacePtr -- mscorlib::_Assembly

-- | 'getType_Assembly' calls mscorlib::_Assembly.GetType_2(BStr name, _Type** result).
getType_Assembly this name = do
  f <- getInterfaceFunction 17 makeGetType_Assembly this
  withBStr name $ \name' ->
    with (nullPtr :: CorLibType) $ \typePtr -> do
      f this name' typePtr >>= checkHR "Assembly.GetType"
      t <- peek typePtr
      if t == nullPtr then error "Assembly.GetType failed"
              else return t

type GetType_Assembly = Assembly -> BStr -> Ptr CorLibType -> IO HResult
foreign import stdcall "dynamic" makeGetType_Assembly :: FunPtr GetType_Assembly -> GetType_Assembly

type CorLibType = InterfacePtr -- mscorlib::_Type

-- | 'invokeMember_Type' calls mscorlib::_Type.InvokeMember_3(BStr name,
--   BindingFlags invokeAttr, _Binder* binder, Variant target, SafeArray* args,
--   Variant* result) to invoke a method without a binder, target or any arguments.
invokeMember_Type this memberName = do
  f <- getInterfaceFunction 57 {- _Type.InvokeMember_3 -} makeInvokeMember_Type this
  withBStr memberName $ \memberName' ->
    with emptyVariant $ \resultPtr -> do
#if x86_64_HOST_ARCH
      with emptyVariant $ \target ->
              f this memberName' 256 nullPtr target nullPtr resultPtr >>= checkHR "Type.InvokeMember"
#else
      f this memberName' 256 nullPtr 0 0 nullPtr resultPtr >>= checkHR "Type.InvokeMember"
#endif
      peek resultPtr

-- Portability note: Type.InvokeMember accepts a by-value variant argument (as its fourth
--                   argument). GHC doesn't let us use that as an argument however, so
--                   there's a calling convention dependent hack in the declarations below.
--                   For Stdcall, this is encoded as two Word64 arguments.
--                   For Win64, it would expect an argument of this size to be passed by reference.
#if x86_64_HOST_ARCH
type InvokeMember_Type = CorLibType -> BStr -> Word32 -> Ptr () -> Ptr Variant -> Ptr () -> Ptr Variant -> IO HResult
#else
type InvokeMember_Type = CorLibType -> BStr -> Word32 -> Ptr () -> Word64 -> Word64 -> Ptr () -> Ptr Variant -> IO HResult
#endif
foreign import stdcall "dynamic" makeInvokeMember_Type :: FunPtr InvokeMember_Type -> InvokeMember_Type


-- | 'loadDriverAndBoot' loads the Salsa driver assembly into the application domain from
--   memory (the binary data is originally stored in 'driverData'), and then invokes the
--   Boot method (from the Salsa.Driver class) to obtain a function pointer for invoking
--   the 'GetPointerToMethod' method.
loadDriverAndBoot :: ICorRuntimeHost -> IO (FunPtr (Ptr Word16 -> IO (FunPtr a)))
loadDriverAndBoot clrHost = do
  -- Obtain an _AppDomain interface pointer to the default application domain
  withInterface (getDefaultDomain_ICorRuntimeHost clrHost) $ \untypedAppDomain -> do
    let iid_AppDomain = Guid 0x05F696DC 0x2B29 0x3663 0xAD 0x8B 0xC4 0x38 0x9C 0xF2 0xA7 0x13
    withInterface (queryInterface untypedAppDomain iid_AppDomain) $ \appDomain -> do

      -- Create a safe array for the contents of the driver assembly binary
      bracket (prim_SafeArrayCreateVector varType_UI1 0 (fromIntegral $ S.length driverData))
          (prim_SafeArrayDestroy)
        (\sa -> do
          -- Copy the driver assembly data into the safe array
          saDataPtr <- with (nullPtr :: Ptr Word8) $ \ptr ->
                                      prim_SafeArrayAccessData sa ptr >> peek ptr
          S.useAsCStringLen driverData $ \(bsPtr,bsLen) -> do
            copyBytes saDataPtr (unsafeCoerce bsPtr) bsLen
          prim_SafeArrayUnaccessData sa

          -- Load the driver assembly into the application domain
          withInterface (load_AppDomain appDomain sa) $ \assembly -> do

            -- Obtain a _Type interface pointer to the Salsa.Driver type
            withInterface (getType_Assembly assembly "Salsa.Driver") $ \typ -> do

              -- Invoke the Boot method of Salsa.Driver to obtain a function
              -- pointer to the 'GetPointerToMethod' method
              (Variant _ returnValue) <- invokeMember_Type typ "Boot"

              -- Return a wrapper function for the 'GetPointerToMethod' method
              return $ unsafeCoerce returnValue)



