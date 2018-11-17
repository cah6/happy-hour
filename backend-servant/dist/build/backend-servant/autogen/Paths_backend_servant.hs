{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_backend_servant (
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

bindir     = "/Users/christian.henry/.cabal/bin"
libdir     = "/Users/christian.henry/.cabal/lib/x86_64-osx-ghc-8.4.3/backend-servant-0.1.0.0-GZ9ALLrZdCpD1BvGquhLhw-backend-servant"
dynlibdir  = "/Users/christian.henry/.cabal/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/christian.henry/.cabal/share/x86_64-osx-ghc-8.4.3/backend-servant-0.1.0.0"
libexecdir = "/Users/christian.henry/.cabal/libexec/x86_64-osx-ghc-8.4.3/backend-servant-0.1.0.0"
sysconfdir = "/Users/christian.henry/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "backend_servant_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "backend_servant_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "backend_servant_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "backend_servant_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "backend_servant_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "backend_servant_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
