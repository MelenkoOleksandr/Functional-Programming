{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_lab2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/parol1111/Desktop/University/FP/lab2/.stack-work/install/aarch64-osx/ce9495d3f1d40be20a36fad2cde67a4786f6f802ae6de6a9d42e1692e87e7037/9.4.8/bin"
libdir     = "/Users/parol1111/Desktop/University/FP/lab2/.stack-work/install/aarch64-osx/ce9495d3f1d40be20a36fad2cde67a4786f6f802ae6de6a9d42e1692e87e7037/9.4.8/lib/aarch64-osx-ghc-9.4.8/lab2-0.1.0.0-1nbfiJWeygqENWz4TkT1Z-lab2"
dynlibdir  = "/Users/parol1111/Desktop/University/FP/lab2/.stack-work/install/aarch64-osx/ce9495d3f1d40be20a36fad2cde67a4786f6f802ae6de6a9d42e1692e87e7037/9.4.8/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/parol1111/Desktop/University/FP/lab2/.stack-work/install/aarch64-osx/ce9495d3f1d40be20a36fad2cde67a4786f6f802ae6de6a9d42e1692e87e7037/9.4.8/share/aarch64-osx-ghc-9.4.8/lab2-0.1.0.0"
libexecdir = "/Users/parol1111/Desktop/University/FP/lab2/.stack-work/install/aarch64-osx/ce9495d3f1d40be20a36fad2cde67a4786f6f802ae6de6a9d42e1692e87e7037/9.4.8/libexec/aarch64-osx-ghc-9.4.8/lab2-0.1.0.0"
sysconfdir = "/Users/parol1111/Desktop/University/FP/lab2/.stack-work/install/aarch64-osx/ce9495d3f1d40be20a36fad2cde67a4786f6f802ae6de6a9d42e1692e87e7037/9.4.8/etc"

getBinDir     = catchIO (getEnv "lab2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "lab2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "lab2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "lab2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lab2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lab2_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
