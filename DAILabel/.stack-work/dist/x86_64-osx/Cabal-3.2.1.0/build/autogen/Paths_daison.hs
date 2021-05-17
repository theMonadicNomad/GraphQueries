{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_daison (
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

bindir     = "/Users/nagacharan/git/GraphQueries/DAILabel/.stack-work/install/x86_64-osx/b8106701b90d6e3f6ccefae9bba2d4cbdefe7e0e16705c8f198f6ab754d07bb0/8.10.4/bin"
libdir     = "/Users/nagacharan/git/GraphQueries/DAILabel/.stack-work/install/x86_64-osx/b8106701b90d6e3f6ccefae9bba2d4cbdefe7e0e16705c8f198f6ab754d07bb0/8.10.4/lib/x86_64-osx-ghc-8.10.4/daison-0.1.0.0-ELCWWLbDprm4sotlkNGbOk"
dynlibdir  = "/Users/nagacharan/git/GraphQueries/DAILabel/.stack-work/install/x86_64-osx/b8106701b90d6e3f6ccefae9bba2d4cbdefe7e0e16705c8f198f6ab754d07bb0/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/nagacharan/git/GraphQueries/DAILabel/.stack-work/install/x86_64-osx/b8106701b90d6e3f6ccefae9bba2d4cbdefe7e0e16705c8f198f6ab754d07bb0/8.10.4/share/x86_64-osx-ghc-8.10.4/daison-0.1.0.0"
libexecdir = "/Users/nagacharan/git/GraphQueries/DAILabel/.stack-work/install/x86_64-osx/b8106701b90d6e3f6ccefae9bba2d4cbdefe7e0e16705c8f198f6ab754d07bb0/8.10.4/libexec/x86_64-osx-ghc-8.10.4/daison-0.1.0.0"
sysconfdir = "/Users/nagacharan/git/GraphQueries/DAILabel/.stack-work/install/x86_64-osx/b8106701b90d6e3f6ccefae9bba2d4cbdefe7e0e16705c8f198f6ab754d07bb0/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "daison_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "daison_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "daison_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "daison_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "daison_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "daison_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
