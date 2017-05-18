--- --------------------------------------------------------------------------
--- Computation of dependendies between Curry modules.
---
--- This module implements the functions to compute the dependency
--- information between Curry modules.
---
--- @author  Björn Peemöller, Fabian Skrlac
--- @version May 2014
--- --------------------------------------------------------------------------
module ModuleDeps (ModuleIdent, Source, Errors, deps) where

import Directory    (doesFileExist, getModificationTime)
import Distribution (defaultParams, setFullPath, setQuiet, setSpecials
                    , stripCurrySuffix, inCurrySubdirModule
                    , FrontendTarget(..), FrontendParams, callFrontendWithParams
                    , installDir
                    )
import FilePath     ( FilePath, dropExtension, takeExtension, takeBaseName
                    , dropTrailingPathSeparator, (</>), (<.>), normalise
                    )
import Files        (lookupFileInPath, getFileInPath)
import FiniteMap    (FM, emptyFM, addToFM, fmToList, lookupFM)
import AnnotatedFlatCurry
import AnnotatedFlatCurryFiles (readTypedFlatCurryFile, typedFlatCurryFileName)
import Function     (second)
import List         (intercalate, partition)
import Maybe        (fromJust, isJust, isNothing)
import Message      (showStatus,showAnalysis)
import Names        (moduleNameToPath)
import System       (system)

import CompilerOpts
import RCFile       (rcValue)
import SCC          (scc)

type ModuleIdent = String
type Errors      = [String]

type Source      = (FilePath, AProg TypeExpr) -- file name, code
type SourceEnv   = FM ModuleIdent (Maybe Source)


--- Compute all dependendies for a given module
--- @param opts - compiler options
--- @param mn   - module name
--- @param fn   - file name of the module (or FlatCurry file name)
--- @return     - topologically sorted list of all dependendies
---               and a list of errors (empty in case of success)
deps :: Options -> ModuleIdent -> FilePath
     -> IO ([(ModuleIdent, Source)], Errors)
deps opts mn fn = do
  mEnv <- sourceDeps opts mn fn (emptyFM (<))
  let (mods1, errs1) = filterMissing mEnv -- handle missing modules
      (mods2, errs2) = flattenDeps mods1  -- check for cyclic imports
                                          -- and sort topologically
  errs3 <- checkTypedFlatCurry mn fn
  return (mods2, concat [errs1, errs2, errs3])

-- Has the given program name a valid typed FlatCurry file?
-- Used to check the result of the front end compilation process.
checkTypedFlatCurry :: ModuleIdent -> String -> IO Errors
checkTypedFlatCurry mname fname
  | isTypedFlatCurryFile fname = return []
  | otherwise             = do
    let tfcyname = stripCurrySuffix (inCurrySubdirModule mname fname) <.> "tfcy"
    existcy  <- doesFileExist fname
    existtfcy <- doesFileExist tfcyname
    if existcy && existtfcy
      then do cymtime  <- getModificationTime fname
              tfcymtime <- getModificationTime tfcyname
              return [ "Typed FlatCurry file " ++ tfcyname ++
                       " is older than Curry file " ++ fname
                     | tfcymtime < cymtime ]
      else return $    [ "Missing Curry file " ++ fname | not existcy ]
                    ++ [ "Missing typed FlatCurry file " ++ tfcyname
                       | not existtfcy ]

moduleDeps :: Options -> SourceEnv -> ModuleIdent -> IO SourceEnv
moduleDeps opts mEnv m = case lookupFM mEnv m of
  Just _  -> return mEnv
  Nothing -> do
    mbFile <- lookupModule opts m
    case mbFile of
      Nothing -> return $ addToFM mEnv m Nothing
      Just fn -> sourceDeps opts { optVerbosity = VerbQuiet } m fn mEnv

lookupModule :: Options -> String -> IO (Maybe FilePath)
lookupModule opts m = lookupFileInPath (moduleNameToPath m)
                      [".curry", ".lcurry", ".tfcy"]
                      (map dropTrailingPathSeparator importPaths)
  where importPaths = "." : optImportPaths opts

sourceDeps :: Options -> ModuleIdent -> String -> SourceEnv -> IO SourceEnv
sourceDeps opts mn fn mEnv = do
  tfcy@(AProg m is _ _ _) <- readCurrySource opts mn fn
  foldIO (moduleDeps opts) (addToFM mEnv m (Just (fn, tfcy))) is

-- Reads a FlatCurry file or parses a Curry module.
-- TODO: This should better return `Either Errors Prog` so that compilation
-- errors can be recognized.
readCurrySource :: Options -> ModuleIdent -> FilePath -> IO (AProg TypeExpr)
readCurrySource opts mn fn
  | isTypedFlatCurryFile fn
  = do showStatus opts $ "Reading directly from typed FlatCurry file '"++fn++"'"
       preprocessTfcyFile {-opts-} fn
  | otherwise
  = do tfcyname <- parseCurryWithOptions opts (stripCurrySuffix mn)
                   $ setFullPath importPaths
                   $ setQuiet    (optVerbosity opts == VerbQuiet)
                   $ setSpecials (optParser opts)
                   defaultParams
       preprocessTfcyFile {-opts-} tfcyname
  where importPaths = "." : optImportPaths opts

-- Pre-process a FlatCurry program and load it for compilation.
-- Currently, the binding optimizer (replace =:=/2 by ==/2) is applied.
preprocessTfcyFile :: {-Options ->-} FilePath -> IO (AProg TypeExpr)
preprocessTfcyFile {-copts-} tfcyname = do
  -- TODO: Reactivate binding optimization when implemented for typed FlatCurry
  -- change current verbosity level to main verbosity level in order to
  -- see the status of pre-processing imported modules:
  {-let opts    = copts { optVerbosity = optMainVerbosity copts }
      rcbopt  = rcValue (rcVars opts) "bindingoptimization"
      optexec = installDir </> "currytools" </> "optimize" </> "bindingopt"
  existsoptexec <- doesFileExist optexec
  when (rcbopt /= "no" && existsoptexec) $ do
    showAnalysis opts $ "Pre-processing file " ++ fcyname
    let verb = case optVerbosity opts of
                  VerbAnalysis -> "-v1"
                  VerbDetails  -> "-v3"
                  _            -> "-v0"
        fastopt = if rcbopt == "full" then "" else "-f"
        optcmd  = unwords [optexec, verb, fastopt, fcyname]
    showAnalysis opts $ "Executing: " ++ optcmd
    status <- system optcmd
    unless (status == 0) $ do
      putStrLn "WARNING: Binding optimization failed for file:"
      putStrLn fcyname-}
  readTypedFlatCurryFile tfcyname

-- Parse a Curry program with the front end and return the FlatCurry file name.
parseCurryWithOptions :: Options -> ModuleIdent -> FrontendParams -> IO String
parseCurryWithOptions opts modname options = do
  mbCurryFile  <- lookupModule opts modname
  unless (isNothing mbCurryFile) $
    callFrontendWithParams TFCY options modname
  liftIO normalise $ getFileInPath (typedFlatCurryFileName modname)
                      [""]
                      (map dropTrailingPathSeparator importPaths)
    where importPaths = "." : optImportPaths opts

isTypedFlatCurryFile :: FilePath -> Bool
isTypedFlatCurryFile fn = takeExtension fn == ".tfcy"

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
    imports (_, (_, (AProg _ imps _ _ _))) = imps

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
