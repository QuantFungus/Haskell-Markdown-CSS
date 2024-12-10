{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Haskell_Markdown_HTML (
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
bindir     = "/Users/Arnav/.cabal/bin"
libdir     = "/Users/Arnav/.cabal/lib/aarch64-osx-ghc-9.4.4/Haskell-Markdown-HTML-0.1.0.0-inplace"
dynlibdir  = "/Users/Arnav/.cabal/lib/aarch64-osx-ghc-9.4.4"
datadir    = "/Users/Arnav/.cabal/share/aarch64-osx-ghc-9.4.4/Haskell-Markdown-HTML-0.1.0.0"
libexecdir = "/Users/Arnav/.cabal/libexec/aarch64-osx-ghc-9.4.4/Haskell-Markdown-HTML-0.1.0.0"
sysconfdir = "/Users/Arnav/.cabal/etc"

getBinDir     = catchIO (getEnv "Haskell_Markdown_HTML_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Haskell_Markdown_HTML_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Haskell_Markdown_HTML_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Haskell_Markdown_HTML_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Haskell_Markdown_HTML_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Haskell_Markdown_HTML_sysconfdir") (\_ -> return sysconfdir)



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
