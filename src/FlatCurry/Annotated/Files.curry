------------------------------------------------------------------------------
--- This library supports meta-programming, i.e., the manipulation of
--- Curry programs in Curry. This library defines I/O actions
--- to read Curry programs and transform them into this representation.
---
--- @author Michael Hanus
--- @version November 2018
------------------------------------------------------------------------------

module FlatCurry.Annotated.Files where

import Directory       ( doesFileExist )
import Distribution    ( inCurrySubdir, stripCurrySuffix
                       , lookupModuleSourceInLoadPath, getLoadPathForModule
                       )
import FileGoodies     ( getFileInPath)
import FilePath        ( takeFileName, (</>), (<.>) )

import System.FrontendExec ( FrontendParams, FrontendTarget (..)
                           , defaultParams, setQuiet, callFrontendWithParams )

import FlatCurry.Annotated.Types

readTypedFlatCurry :: String -> IO (AProg TypeExpr)
readTypedFlatCurry progname =
   readTypedFlatCurryWithParseOptions progname (setQuiet True defaultParams)

readTypedFlatCurryWithParseOptions :: String -> FrontendParams -> IO (AProg TypeExpr)
readTypedFlatCurryWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileInPath (typedFlatCurryFileName (takeFileName progname)) [""]
                                loadpath
      readTypedFlatCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams TFCY options progname
      readTypedFlatCurryFile (typedFlatCurryFileName (dir </> takeFileName progname))

typedFlatCurryFileName :: String -> String
typedFlatCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "tfcy"

readTypedFlatCurryFile :: String -> IO (AProg TypeExpr)
readTypedFlatCurryFile filename = do
  filecontents <- readTypedFlatCurryFileRaw filename
  return (read filecontents)

readTypedFlatCurryFileRaw :: String -> IO String
readTypedFlatCurryFileRaw filename = do
  extfcy <- doesFileExist filename
  if extfcy
   then readFile filename
   else do let subdirfilename = inCurrySubdir filename
           exdirtfcy <- doesFileExist subdirfilename
           if exdirtfcy
            then readFile subdirfilename
            else error ("EXISTENCE ERROR: Typed FlatCurry file '" ++ filename ++
                        "' does not exist")
