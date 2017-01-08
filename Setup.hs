import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
-- import Distribution.InstalledPackageInfo
import Distribution.Simple.Program
import Distribution.Simple.GHC
import Distribution.Simple.Configure
import Distribution.Verbosity

import qualified Distribution.Simple.PackageIndex as Pkg

import System.Exit
import System.IO
import Data.IORef
import Data.Char
import Data.Maybe
import System.Directory

main = defaultMainWithHooks simpleUserHooks {
                      postConf    = addPath
                     }
  
addPath :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
addPath args flags pkg_descr lbi = do
    let verbosity = fromFlag (configVerbosity flags)      
    -- :pkg_path <- pkgRoot verbosity lbi 
        msg = (show $ libBuildInfo $ fromJust $ library pkg_descr) ++ "\n" ++ (show $ map testBuildInfo $ testSuites pkg_descr)
    paths <- mapM (pkgRoot verbosity lbi) (withPackageDB lbi) 
    writeFile file $ show paths
    -- let buildinfo = emptyBuildInfo{
    --          cppOptions = ["-DPACKAGE_PATH=" ++ show pkg_path,
    --                        "-DGHC_PATHS_GHC=" ++ show c_ghc,
    --                        "-DGHC_PATHS_LIBDIR=" ++ show libdir,
    --                        "-DGHC_PATHS_DOCDIR=" ++ show docdir ]
    --        }
    -- writeFile file (show buildinfo)
    checkForeignDeps pkg_descr lbi (lessVerbose verbosity)

readHook :: Args -> a -> IO HookedBuildInfo
readHook _ _ = do
  str <- readFile file
  return (Just (read str), [])

file = "blaa.info"
