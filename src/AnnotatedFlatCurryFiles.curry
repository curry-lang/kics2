------------------------------------------------------------------------------
--- This library supports meta-programming, i.e., the manipulation of
--- Curry programs in Curry. This library defines I/O actions
--.  to read Curry programs and transform them into this representation.
---
--- @author Michael Hanus
--- @version October 2015
--- @category meta
------------------------------------------------------------------------------

module AnnotatedFlatCurryFiles where

import Directory       (doesFileExist)
import Distribution    ( FrontendParams, FrontendTarget (..), defaultParams
                       , setQuiet, inCurrySubdir, stripCurrySuffix
                       , callFrontend, callFrontendWithParams
                       , lookupModuleSourceInLoadPath, getLoadPathForModule
                       )
import FileGoodies     (getFileInPath, lookupFileInPath)
import FilePath        (takeFileName, (</>), (<.>))
import AnnotatedFlatCurry
import Maybe           (isNothing)
import ReadShowTerm    (readUnqualifiedTerm, showTerm)

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
  extfcy <- doesFileExist filename
  if extfcy
   then readExistingTFCY filename
   else do let subdirfilename = inCurrySubdir filename
           exdirtfcy <- doesFileExist subdirfilename
           if exdirtfcy
            then readExistingTFCY subdirfilename
            else error ("EXISTENCE ERROR: Typed FlatCurry file '" ++ filename ++
                        "' does not exist")
 where
   readExistingTFCY fname = do
     filecontents <- readFile fname
     return (read filecontents)

