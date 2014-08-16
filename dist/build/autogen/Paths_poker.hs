module Paths_poker (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/goshakkk/.cabal/bin"
libdir     = "/Users/goshakkk/.cabal/lib/x86_64-osx-ghc-7.8.3/poker-0.1.0.0"
datadir    = "/Users/goshakkk/.cabal/share/x86_64-osx-ghc-7.8.3/poker-0.1.0.0"
libexecdir = "/Users/goshakkk/.cabal/libexec"
sysconfdir = "/Users/goshakkk/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "poker_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "poker_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "poker_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "poker_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "poker_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
