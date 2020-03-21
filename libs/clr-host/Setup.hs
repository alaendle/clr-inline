import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Configure(configure)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Program
import Distribution.Verbosity as Verbosity

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.Directory
import System.FilePath

main = defaultMainWithHooks simpleUserHooks { confHook  = configureClrHost
                                            , buildHook = buildClrHost     }

buildClrHost :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildClrHost pd lbi hooks bflags = do
  let verbosity = fromFlagOrDefault Verbosity.normal (buildVerbosity bflags)
  let progDb = withPrograms lbi
  currentDir <- getCurrentDirectory
  let src = currentDir </> "src" </> "Driver.cs"
  let proj = currentDir </> "src" </> "Driver.csproj"
  runDbProgram verbosity csharpCompiler progDb ["build", "--nologo", "--configuration", "Release", "--output", currentDir </> "src", proj]
  buildHook simpleUserHooks pd lbi hooks bflags

configureClrHost :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
configureClrHost (gpd,hbi) cf = do
  lbi <- confHook simpleUserHooks (gpd, hbi) cf
  let verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity (configFlags lbi))
  (cfgdProg, progDb) <- requireProgram verbosity csharpCompiler (withPrograms lbi)
  let lbi'   = lbi { withPrograms = progDb }  :: LocalBuildInfo
  return lbi'

cscFindLocation :: Verbosity -> ProgramSearchPath -> IO (Maybe (FilePath, [FilePath]))
cscFindLocation verb path = runMaybeT $ findExec "dotnet"
                          where findExec name = MaybeT $ findProgramOnSearchPath verb path name

csharpCompiler :: Program
csharpCompiler = (simpleProgram "dotnet") { programFindLocation = cscFindLocation }

