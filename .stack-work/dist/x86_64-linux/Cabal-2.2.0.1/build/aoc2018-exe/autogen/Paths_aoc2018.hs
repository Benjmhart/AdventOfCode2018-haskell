{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_aoc2018 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bhart/projects/advent/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/bin"
libdir     = "/home/bhart/projects/advent/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/lib/x86_64-linux-ghc-8.4.4/aoc2018-0.1.0.0-95WowAF3ZTKFWvs7qNui0J-aoc2018-exe"
dynlibdir  = "/home/bhart/projects/advent/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/bhart/projects/advent/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/share/x86_64-linux-ghc-8.4.4/aoc2018-0.1.0.0"
libexecdir = "/home/bhart/projects/advent/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/libexec/x86_64-linux-ghc-8.4.4/aoc2018-0.1.0.0"
sysconfdir = "/home/bhart/projects/advent/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aoc2018_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aoc2018_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aoc2018_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aoc2018_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc2018_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc2018_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
