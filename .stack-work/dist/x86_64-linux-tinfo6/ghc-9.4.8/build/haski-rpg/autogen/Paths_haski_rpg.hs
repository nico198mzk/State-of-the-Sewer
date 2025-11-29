{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haski_rpg (
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
bindir     = "/mnt/c/Users/marti/Desktop/State-of-the-Sewer/.stack-work/install/x86_64-linux-tinfo6/7cacff2ee8fccd071abdb81232d6787bfe58fc531e19ac13a3e6037d38166151/9.4.8/bin"
libdir     = "/mnt/c/Users/marti/Desktop/State-of-the-Sewer/.stack-work/install/x86_64-linux-tinfo6/7cacff2ee8fccd071abdb81232d6787bfe58fc531e19ac13a3e6037d38166151/9.4.8/lib/x86_64-linux-ghc-9.4.8/haski-rpg-0.1.0.0-8H4ppW3Iq7o7WLmJzwCH9l-haski-rpg"
dynlibdir  = "/mnt/c/Users/marti/Desktop/State-of-the-Sewer/.stack-work/install/x86_64-linux-tinfo6/7cacff2ee8fccd071abdb81232d6787bfe58fc531e19ac13a3e6037d38166151/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/mnt/c/Users/marti/Desktop/State-of-the-Sewer/.stack-work/install/x86_64-linux-tinfo6/7cacff2ee8fccd071abdb81232d6787bfe58fc531e19ac13a3e6037d38166151/9.4.8/share/x86_64-linux-ghc-9.4.8/haski-rpg-0.1.0.0"
libexecdir = "/mnt/c/Users/marti/Desktop/State-of-the-Sewer/.stack-work/install/x86_64-linux-tinfo6/7cacff2ee8fccd071abdb81232d6787bfe58fc531e19ac13a3e6037d38166151/9.4.8/libexec/x86_64-linux-ghc-9.4.8/haski-rpg-0.1.0.0"
sysconfdir = "/mnt/c/Users/marti/Desktop/State-of-the-Sewer/.stack-work/install/x86_64-linux-tinfo6/7cacff2ee8fccd071abdb81232d6787bfe58fc531e19ac13a3e6037d38166151/9.4.8/etc"

getBinDir     = catchIO (getEnv "haski_rpg_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haski_rpg_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haski_rpg_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haski_rpg_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haski_rpg_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haski_rpg_sysconfdir") (\_ -> return sysconfdir)




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
