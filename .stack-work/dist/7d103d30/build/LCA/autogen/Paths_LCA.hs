{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_LCA (
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

bindir     = "D:\\Users\\Calum\\Documents\\GitHub\\CS3012-LCA-Cacomerf\\.stack-work\\install\\0f3b3bca\\bin"
libdir     = "D:\\Users\\Calum\\Documents\\GitHub\\CS3012-LCA-Cacomerf\\.stack-work\\install\\0f3b3bca\\lib\\x86_64-windows-ghc-8.4.3\\LCA-0.1.0.0-K9FdpJ8mfQx96L7MJwk87C-LCA"
dynlibdir  = "D:\\Users\\Calum\\Documents\\GitHub\\CS3012-LCA-Cacomerf\\.stack-work\\install\\0f3b3bca\\lib\\x86_64-windows-ghc-8.4.3"
datadir    = "D:\\Users\\Calum\\Documents\\GitHub\\CS3012-LCA-Cacomerf\\.stack-work\\install\\0f3b3bca\\share\\x86_64-windows-ghc-8.4.3\\LCA-0.1.0.0"
libexecdir = "D:\\Users\\Calum\\Documents\\GitHub\\CS3012-LCA-Cacomerf\\.stack-work\\install\\0f3b3bca\\libexec\\x86_64-windows-ghc-8.4.3\\LCA-0.1.0.0"
sysconfdir = "D:\\Users\\Calum\\Documents\\GitHub\\CS3012-LCA-Cacomerf\\.stack-work\\install\\0f3b3bca\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "LCA_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "LCA_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "LCA_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "LCA_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "LCA_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "LCA_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
