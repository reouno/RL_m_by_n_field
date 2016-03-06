module Paths_RL005nbym (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/reo/hsproject/RL005mbyn/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/bin"
libdir     = "/Users/reo/hsproject/RL005mbyn/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/lib/x86_64-osx-ghc-7.10.3/RL005nbym-0.1.0.0-9lGNV08hgas9M5W10C6mJ9"
datadir    = "/Users/reo/hsproject/RL005mbyn/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/share/x86_64-osx-ghc-7.10.3/RL005nbym-0.1.0.0"
libexecdir = "/Users/reo/hsproject/RL005mbyn/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/libexec"
sysconfdir = "/Users/reo/hsproject/RL005mbyn/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "RL005nbym_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "RL005nbym_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "RL005nbym_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "RL005nbym_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "RL005nbym_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
