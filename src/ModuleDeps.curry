--- --------------------------------------------------------------------------
--- Computation of dependendies between Curry modules.
---
--- This module implements the functions to compute the dependency
--- information between Curry modules.
---
--- @author  Björn Peemöller, Fabian Skrlac
--- @version May 2014
--- --------------------------------------------------------------------------
{-# LANGUAGE Records #-}
module ModuleDeps (ModuleIdent, Source, Errors, deps) where

import Directory    (doesFileExist, getModificationTime)
import Distribution (defaultParams, setFullPath, setQuiet, setSpecials
                    , lookupFileInLoadPath, findFileInLoadPath
                    , FrontendTarget(..), FrontendParams, callFrontendWithParams
                    , installDir
                    )
import FilePath     ( FilePath, dropExtension, takeExtension, takeBaseName
                    , dropTrailingPathSeparator, (</>), (<.>), normalise
                    )
import Files        (lookupFileInPath)
import FiniteMap    (FM, emptyFM, addToFM, fmToList, lookupFM)
import FlatCurry    ( Prog (..), readFlatCurryFile, flatCurryFileName
                    , readFlatCurryWithParseOptions
                    )
import Function     (second)
import List         (intercalate, partition)
import Maybe        (fromJust, isJust, isNothing)
import Message      (showStatus)
import Names        (splitIdentifiers)
import System       (system)

import CompilerOpts
import RCFile       (rcValue)
import SCC          (scc)

type ModuleIdent = String
type Errors      = [String]

type Source      = (FilePath, Prog) -- file name, code
type SourceEnv   = FM ModuleIdent (Maybe Source)


--- Compute all dependendies for a given module
--- @param opts - compiler options
--- @param fn   - file name of the module
--- @return     - topologically sorted list of all dependendies
---               and a list of errors (empty in case of success)
deps :: Options -> FilePath -> IO ([(ModuleIdent, Source)], Errors)
deps opts fn = do
  mEnv <- sourceDeps opts (fileNameToModuleIdent fn) fn (emptyFM (<))
  fcyvalid <- isFlatCurryValid fn
  let (mods1, errs1) = filterMissing mEnv -- handle missing modules
      (mods2, errs2) = flattenDeps mods1  -- check for cyclic imports
                                          -- and sort topologically
      errs3 | fcyvalid  = []
            | otherwise = ["Compilation aborted"]
  return (mods2, concat [errs1, errs2, errs3])

--- Extract the `ModuleIdent` from a `FilePath`.
fileNameToModuleIdent :: FilePath -> ModuleIdent
fileNameToModuleIdent fn = dropExtension $ takeBaseName fn

-- Has the given program name a valid FlatCurry file?
-- Used to check the result of the front end.
isFlatCurryValid :: String -> IO Bool
isFlatCurryValid fname
  | isFlatCurryFile fname = return True
  | otherwise             = do
    let fcyname = flatCurryFileName fname
    existcy  <- doesFileExist fname
    existfcy <- doesFileExist fcyname
    if existcy && existfcy
      then do cymtime  <- getModificationTime fname
              fcymtime <- getModificationTime fcyname
              return (fcymtime >= cymtime)
      else return False

moduleDeps :: Options -> SourceEnv -> ModuleIdent -> IO SourceEnv
moduleDeps opts mEnv m = case lookupFM mEnv m of
  Just _  -> return mEnv
  Nothing -> do
    mbFile <- lookupModule opts m
    case mbFile of
      Nothing -> return $ addToFM mEnv m Nothing
      Just fn -> sourceDeps { optVerbosity := VerbQuiet | opts } m fn mEnv

lookupModule :: Options -> String -> IO (Maybe FilePath)
lookupModule opts m = lookupFileInPath (moduleNameToFile m)
                      [".curry", ".lcurry", ".fcy"]
                      (map dropTrailingPathSeparator importPaths)
  where importPaths = "." : opts :> optImportPaths

moduleNameToFile :: String -> FilePath
moduleNameToFile = foldr1 (</>) . splitIdentifiers

sourceDeps :: Options -> ModuleIdent -> String -> SourceEnv -> IO SourceEnv
sourceDeps opts m fn mEnv = do
  fcy@(Prog _ is _ _ _) <- readCurrySource opts fn
  foldIO (moduleDeps opts) (addToFM mEnv m (Just (fn, fcy))) is

-- TODO: This should better return `Either Errors Prog` so that compilation
-- errors can be recognized.
readCurrySource :: Options -> FilePath -> IO Prog
readCurrySource opts fn
  | isFlatCurryFile fn
  = preprocessFcyFile opts fn
  | otherwise
  = do fcyname <- parseCurryWithOptions (dropExtension fn)
                  $ setFullPath importPaths
                  $ setQuiet    (opts :> optVerbosity == VerbQuiet)
                  $ setSpecials (opts :> optParser)
                  defaultParams
       preprocessFcyFile opts fcyname
  where importPaths = "." : opts :> optImportPaths

-- Pre-process a FlatCurry program and load it for compilation.
-- Currently, the binding optimizer (replace =:=/2 by ==/2) is applied.
preprocessFcyFile :: Options -> FilePath -> IO Prog
preprocessFcyFile copts fcyname = do
  -- change current verbosity level to main verbosity level in order to
  -- see the status of pre-processing imported modules:
  let opts = { optVerbosity := opts :> optMainVerbosity | copts}
  showStatus opts $ "Pre-processing file " ++ fcyname
  let rcbopt  = rcValue (opts :> rcVars) "bindingoptimization"
  unless (rcbopt == "no") $ do
    let optexec = installDir </> "currytools" </> "optimize" </> "bindingopt"
    existsoptexec <- doesFileExist optexec
    when existsoptexec $ do
     let cmpVerb = opts :> optVerbosity
         verb    = case cmpVerb of
                     VerbStatus   -> "-v1"
                     VerbAnalysis -> "-v3"
                     _            -> "-v0"
         fastopt = if rcbopt == "full" then "" else "-f"
     let optcmd = unwords [optexec, verb, fastopt, fcyname]
     showStatus opts $ "Executing: "++ optcmd
     status <- system optcmd
     when (status > 0) $ do
       putStrLn "WARNING: no binding optimization performed for file:"
       putStrLn fcyname
  readFlatCurryFile fcyname

-- Parse a Curry program with the front end and return the FlatCurry file name.
parseCurryWithOptions :: String -> FrontendParams -> IO String
parseCurryWithOptions progname options = do
  mbCurryFile  <- lookupFileInLoadPath (progname <.> "curry")
  mbLCurryFile <- lookupFileInLoadPath (progname <.> "lcurry")
  unless (isNothing mbCurryFile && isNothing mbLCurryFile) $
    callFrontendWithParams FCY options progname
  liftIO normalise $ findFileInLoadPath (progname <.> "fcy")

isFlatCurryFile :: FilePath -> Bool
isFlatCurryFile fn = takeExtension fn == ".fcy"

filterMissing :: SourceEnv -> ([(ModuleIdent, Source)], Errors)
filterMissing env = (map (second fromJust) present, errs) where
  errs = map (\(m, _) -> "Module " ++ m ++ " could not be found") missing
  (present, missing) = partition (isJust . snd) $ fmToList env

--- Convert the dependency map into a topologically sorted dependency list
--- and a list of errors for cyclic imports.
flattenDeps :: [(ModuleIdent, Source)] -> ([(ModuleIdent, Source)], Errors)
flattenDeps = fdeps . sortDeps where

  sortDeps :: [(ModuleIdent, Source)] -> [[(ModuleIdent, Source)]]
  sortDeps = scc modules imports where
    -- extract the module ident
    modules (m, _) = [m]
    -- extract the imports
    imports (_, (_, (Prog _ imps _ _ _))) = imps

  fdeps :: [[(ModuleIdent, Source)]] -> ([(ModuleIdent, Source)], Errors)
  fdeps = foldr checkdep ([], [])

  checkdep []          (ms', errs) = (ms'  , errs)
  checkdep [m]         (ms', errs) = (m:ms', errs)
  checkdep dep@(_:_:_) (ms', errs) = (ms'  , cyclicError (map fst dep) : errs)

  cyclicError :: [ModuleIdent] -> String
  cyclicError ms = "Cylic import dependency between modules " ++
                   intercalate ", " inits ++ " and " ++ last where
    (inits, last)      = splitLast ms
    splitLast []       = error "ModuleDeps.splitLast: empty list"
    splitLast (x:[])   = ([]  , x)
    splitLast (x:y:ys) = (x:xs, z) where (xs, z) = splitLast (y:ys)
