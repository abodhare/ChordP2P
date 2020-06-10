{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ChordP2P (
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

bindir     = "/home/oga_tatsumi/.cabal/bin"
libdir     = "/home/oga_tatsumi/.cabal/lib/x86_64-linux-ghc-8.8.3/ChordP2P-0.1.0.0-inplace-ChordP2P-exe"
dynlibdir  = "/home/oga_tatsumi/.cabal/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/oga_tatsumi/.cabal/share/x86_64-linux-ghc-8.8.3/ChordP2P-0.1.0.0"
libexecdir = "/home/oga_tatsumi/.cabal/libexec/x86_64-linux-ghc-8.8.3/ChordP2P-0.1.0.0"
sysconfdir = "/home/oga_tatsumi/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ChordP2P_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ChordP2P_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ChordP2P_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ChordP2P_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ChordP2P_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ChordP2P_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
