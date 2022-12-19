{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_mancala (
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
bindir     = "/Users/chanceonyiorah/Documents/Fall2022/COMS4995/Minimax_Mancala/.stack-work/install/x86_64-osx/e728fbc0444a73dd247644d86590f18168acbd281173e2bb27d1159106974116/9.2.5/bin"
libdir     = "/Users/chanceonyiorah/Documents/Fall2022/COMS4995/Minimax_Mancala/.stack-work/install/x86_64-osx/e728fbc0444a73dd247644d86590f18168acbd281173e2bb27d1159106974116/9.2.5/lib/x86_64-osx-ghc-9.2.5/mancala-0.1.0.0-KYVjh9IuNAd7QuN1eDeYqi"
dynlibdir  = "/Users/chanceonyiorah/Documents/Fall2022/COMS4995/Minimax_Mancala/.stack-work/install/x86_64-osx/e728fbc0444a73dd247644d86590f18168acbd281173e2bb27d1159106974116/9.2.5/lib/x86_64-osx-ghc-9.2.5"
datadir    = "/Users/chanceonyiorah/Documents/Fall2022/COMS4995/Minimax_Mancala/.stack-work/install/x86_64-osx/e728fbc0444a73dd247644d86590f18168acbd281173e2bb27d1159106974116/9.2.5/share/x86_64-osx-ghc-9.2.5/mancala-0.1.0.0"
libexecdir = "/Users/chanceonyiorah/Documents/Fall2022/COMS4995/Minimax_Mancala/.stack-work/install/x86_64-osx/e728fbc0444a73dd247644d86590f18168acbd281173e2bb27d1159106974116/9.2.5/libexec/x86_64-osx-ghc-9.2.5/mancala-0.1.0.0"
sysconfdir = "/Users/chanceonyiorah/Documents/Fall2022/COMS4995/Minimax_Mancala/.stack-work/install/x86_64-osx/e728fbc0444a73dd247644d86590f18168acbd281173e2bb27d1159106974116/9.2.5/etc"

getBinDir     = catchIO (getEnv "mancala_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "mancala_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "mancala_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "mancala_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mancala_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mancala_sysconfdir") (\_ -> return sysconfdir)




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
