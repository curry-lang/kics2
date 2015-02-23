--- --------------------------------------------------------------------------
--- This module implements the "linker" of KiCS2.
--- It provides operations to write the main goal as a Haskell file
--- and compiling this main file together with all compiled Curry modules.
---
--- @author Michael Hanus, Bjoern Peemoeller
--- @version January 2014
--- --------------------------------------------------------------------------
module Linker
  ( ReplState (..), NonDetMode (..), MainCompile (..), loadPaths
  , setExitStatus
  , writeVerboseInfo, mainGoalFile, mainModuleIdent, initReplState
  , createAndCompileMain
  , getTimeCmd
  ) where

import AbstractCurry
import Directory
import FilePath      ((</>), dropExtension)
import IO            (Handle, hFlush, hGetContents, hClose, stdout)
import IOExts        (execCmd)
import List          (intercalate, isInfixOf)
import PropertyFile
import ReadShowTerm  (readQTermFile)
import System

import qualified Installation as Inst
import GhciComm
import Names         (funcInfoFile)
import RCFile
import Utils         (notNull, strip)

data ReplState = ReplState
  { kics2Home    :: String     -- installation directory of the system
  , rcvars       :: [(String, String)] -- content of rc file
  , idSupply     :: String     -- IDSupply implementation (ioref, integer or ghc)
  , verbose      :: Int        -- verbosity level:
                               -- 0 = errors and warnings
                               -- 1 = show frontend compilation status
                               -- 2 = show also kics2c compilation status
                               -- 3 = show also ghc compilation status
                               -- 4 = show analysis information
  , importPaths  :: [String]   -- additional directories to search for imports
  , libPaths     :: [String]   -- directories containg the standard libraries
  , preludeName  :: String     -- the name of the standard prelude
  , outputSubdir :: String
  , mainMod      :: String     -- name of main module
  , addMods      :: [String]   -- names of additionally added modules
  , prompt       :: String     -- repl prompt shown in front of user input
  , optim        :: Bool       -- compile with optimization
  , ndMode       :: NonDetMode -- mode for non-deterministic main goal
  , firstSol     :: Bool       -- print only first solution to nd main goal?
  , interactive  :: Bool       -- interactive execution of goal?
  , showBindings :: Bool       -- show free variables in main goal in output?
  , showTime     :: Bool       -- show execution of main goal?
  , traceFailure :: Bool       -- trace failure in deterministic expression
  , profile      :: Bool       -- use GHC's profiling capabilities
  , useGhci      :: Bool       -- use ghci to evaluate main goal
  , safeExec     :: Bool       -- safe execution mode without I/O actions
  , parseOpts    :: String     -- additional options for the front end
  , cmpOpts      :: String     -- additional options for calling kics2 compiler
  , ghcOpts      :: String     -- additional options for ghc compilation
  , rtsOpts      :: String     -- run-time options for ghc
  , rtsArgs      :: String     -- run-time arguments passed to main application
  , quit         :: Bool       -- terminate the REPL?
  , exitStatus   :: Int        -- exit status (set in case of REPL errors)
  , sourceguis   :: [(String,(String,Handle))] -- handles to SourceProgGUIs
  , ghcicomm     :: Maybe GhciComm -- possible ghci comm. info
  }

--- Initial state of REPL
initReplState :: ReplState
initReplState = ReplState
  { kics2Home    = ""
  , rcvars       = []
  , idSupply     = "ioref"
  , verbose      = 1
  , importPaths  = []
  , libPaths     = map (Inst.installDir </>) ["lib", "lib" </> "meta"]
  , preludeName  = "Prelude"
  , outputSubdir = ".curry" </> "kics2"
  , mainMod      = "Prelude"
  , addMods      = []
  , prompt       = "%s> "
  , optim        = True
  , ndMode       = BFS
  , firstSol     = False
  , interactive  = False
  , showBindings = True
  , showTime     = False
  , traceFailure = False
  , profile      = False
  , useGhci      = False
  , safeExec     = False
  , parseOpts    = ""
  , cmpOpts      = ""
  , ghcOpts      = ""
  , rtsOpts      = ""
  , rtsArgs      = ""
  , quit         = False
  , exitStatus   = 0
  , sourceguis   = []
  , ghcicomm     = Nothing
  }

loadPaths :: ReplState -> [String]
loadPaths rst = "." : importPaths rst ++ libPaths rst

--- Sets the exit status in the REPL state.
setExitStatus :: Int -> ReplState -> ReplState
setExitStatus s rst = rst { exitStatus = s }

-- ---------------------------------------------------------------------------

--- File name of created Main file
mainGoalFile :: String
mainGoalFile = "Curry_Main_Goal.curry"

--- Module identifier for Main
mainModuleIdent :: String
mainModuleIdent = "Curry_Main_Goal"

--- Show an info message for a given verbosity level
writeVerboseInfo :: ReplState -> Int -> String -> IO ()
writeVerboseInfo rst lvl msg =
  unless (verbose rst < lvl) (putStrLn msg >> hFlush stdout)

--- Reads the determinism infomation for the main goal file
readInfoFile :: ReplState -> IO [((String,String),Bool)]
readInfoFile rst = do
  readQTermFile (funcInfoFile (outputSubdir rst) mainModuleIdent mainGoalFile)

getGoalInfo :: ReplState -> IO (Bool, Bool)
getGoalInfo rst = do
  infos <- readInfoFile rst
  --print infos
  let isdet = notNull (filter (\i -> (snd (fst i)) == "d_C_kics2MainGoal")
                                infos)
      isio  = snd (head (filter (\i -> snd (fst i) ==
                           (if isdet then "d" else "nd") ++ "_C_kics2MainGoal")
                        infos))
  writeVerboseInfo rst 2 $ "Initial goal is " ++
                (if isdet then "" else "non-") ++ "deterministic and " ++
                (if isio  then "" else "not ") ++ "of IO type..."
  return (isdet, isio)

--- Checks whether user-defined ghc options have been changed.
updateGhcOptions :: ReplState -> IO (ReplState, Bool)
updateGhcOptions rst =
  if oldOpts == newOpts
    then return (rst,False)
    else do
      setRCProperty key newOpts
      rcDefs <- readRC
      return (rst { rcvars = rcDefs }, True)
 where
   key = "ghc_options"
   oldOpts = rcValue (rcvars rst) key
   newOpts = ghcOpts rst

--- Result of compiling main program
data MainCompile = MainError | MainDet | MainNonDet

--- Create and compile the main module containing the main goal
createAndCompileMain :: ReplState -> Bool -> String -> Maybe Int
                     -> IO (ReplState, MainCompile)
createAndCompileMain rst createExecutable mainExp bindings = do
  (isdet, isio) <- getGoalInfo rst
  (rst',wasUpdated) <- updateGhcOptions rst
  writeFile mainFile $ mainModule rst' isdet isio (traceFailure rst) bindings

  let ghcCompile = ghcCall rst' useGhci wasUpdated mainFile
  tghcCompile <- getTimeCmd rst' "GHC compilation" ghcCompile
  writeVerboseInfo rst' 3 $ "Compiling " ++ mainFile ++ " with: " ++ tghcCompile
  (rst'', status) <- if useGhci
                      then compileWithGhci rst' ghcCompile mainExp
                      else system tghcCompile >>= \stat -> return (rst', stat)
  return (if status > 0
          then (setExitStatus 1 rst'', MainError)
          else (setExitStatus 0 rst'',
                if isdet || isio then MainDet else MainNonDet))
 where
  mainFile = "." </> outputSubdir rst </> "Main.hs"
  -- option parsing
  useGhci = useGhci rst && not createExecutable && not (interactive rst)

compileWithGhci :: ReplState -> String -> String -> IO (ReplState, Int)
compileWithGhci rst ghcCompile mainExp = do
  comm <- refresh ghcCompile (verbose rst > 2)
  writeVerboseInfo rst 1 $ "Evaluating expression: " ++ strip mainExp
  evalMainCmd comm (showTime rst)
  return (rst { ghcicomm = Just comm }, 0)
    where refresh = case ghcicomm rst of
                    Nothing  -> initGhciComm
                    Just old -> restartGhciComm old

ghcCall :: ReplState -> Bool -> Bool -> String -> String
ghcCall rst useGhci recompile mainFile = unwords . filter notNull $
  [ Inst.ghcExec
  , Inst.ghcOptions
  , if optim rst && not useGhci then "-O2"               else ""
  , if useGhci                  then "--interactive"     else "--make"
  , if verbose rst < 2          then "-v0"               else "-v1"
  , if withGhcSupply            then "-package ghc"      else ""
  , if isParSearch              then "-threaded"         else ""
  , if withProfiling            then "-prof -fprof-auto" else ""
  , if withRtsOpts              then "-rtsopts"          else ""
  , if recompile                then "-fforce-recomp"    else ""
      -- XRelaxedPolyRec due to problem in FlatCurryShow
  , "-XMultiParamTypeClasses", "-XFlexibleInstances", "-XRelaxedPolyRec"
  , ghcOpts rst
  , "-i" ++ (intercalate ":" ghcImports)
  , mainFile
  ]
 where
  withGhcSupply = (idSupply rst) `elem` ["ghc", "ioref"]
  withRtsOpts   = notNull (rtsOpts rst) || isParSearch || withProfiling
  withProfiling = profile rst
  isParSearch   = case ndMode rst of
    Par _ -> True
    _     -> False
  ghcImports
    | Inst.installGlobal
    = map (</> outputSubdir rst) ("." : importPaths rst)
    | otherwise
    = [ kics2Home rst </> "runtime"
      , kics2Home rst </> "runtime" </> "idsupply" ++ idSupply rst
      ] ++ map (</> outputSubdir rst) (loadPaths rst)

--- Mode of non-deterministic evaluation of main goal
data NonDetMode  = DFS | BFS | IDS Int | Par Int | PrDFS | PrtChoices Int | DEBUG

data EvalMode    = All | One | Interactive MoreDefault -- | Count

data MoreDefault = MoreYes | MoreNo | MoreAll

-- Create the Main.hs program containing the call to the initial expression:
mainModule :: ReplState -> Bool -> Bool -> Bool -> Maybe Int -> String
mainModule rst isdet isio isTF mbBindings = unlines
  [ "module Main where"
  , if interactive rst then "import MonadList" else ""
  , "import Basics"
  , "import SafeExec"
  , if mbBindings==Nothing
    then ""
    else ("import Curry_"++ preludeName rst)
  , if (traceFailure rst)
      then "import Curry_Trace_" ++ dropExtension mainGoalFile
      else "import Curry_" ++ dropExtension mainGoalFile
  , ""
  , "main :: IO ()"
  , mainExpr "kics2MainGoal" isdet isio isTF (ndMode rst) evalMode mbBindings
  ]
 where
  evalMode
    | interactive rst = Interactive moreDefault
    | firstSol rst    = One
    | otherwise          = All
  moreDefault = case rcValue (rcvars rst) "moresolutions" of
    "yes" -> MoreYes
    "no"  -> MoreNo
    "all" -> MoreAll
    _     -> MoreYes

mainExpr :: String -> Bool -> Bool -> Bool -> NonDetMode -> EvalMode -> Maybe Int -> String
mainExpr goal isdet isio isTF ndMode evalMode mbBindings
  = "main = " ++ mainOperation ++ ' ' : detPrefix ++ goal
 where
  detPrefix = if isdet then "d_C_" else "nd_C_"
  mainOperation
    | isio && isdet && isTF = "failtraceDIO"
    | isio && isdet         = "evalDIO"
    | isio                  = "evalIO"
    | isdet && isTF         = "failtraceD"
    | isdet                 = "evalD"
    | otherwise             = case ndMode of
      DEBUG        -> searchExpr $ "debugSearch"
      PrDFS        -> searchExpr $ "prdfs"
      DFS          -> searchExpr $ "printDFS" ++ searchSuffix
      BFS          -> searchExpr $ "printBFS" ++ searchSuffix
      IDS        d -> searchExpr $ "printIDS" ++ searchSuffix ++ ' ' : show d
      Par        _ -> searchExpr $ "printPar" ++ searchSuffix
      PrtChoices d -> "prtChoiceTree" ++ ' ' : show d
  searchExpr strat = strat ++ ' ' : printOperation
  searchSuffix = case evalMode of
    All            -> ""
    One            -> "1"
    Interactive md -> "i " ++ show md
  printOperation = maybe "print" printWithBindings mbBindings

  -- Create the following Haskell expression for printing goals with bindings:
  -- (\ (OP_Tuple<n+2> result names v1 ... v<n>) ->
  --  printWithBindings (zip (fromCurry names) [show v1,..., show v<n>]) result)
  printWithBindings n =
    "(\\ (OP_Tuple" ++ show (n + 2) ++ " result names" ++
    concatMap ((" v" ++) . show) [1 .. n] ++
    " ) ->" ++
    " printWithBindings (zip (fromCurry names) [" ++
    intercalate "," (map (("show v" ++) . show) [1..n]) ++
    "]) result)"


---------------------------------------------------------------------------
-- Auxiliaries:

-- Decorates a shell command so that timing information is shown if
-- the corresponding option is set.
getTimeCmd :: ReplState -> String -> String -> IO String
getTimeCmd rst timename cmd
  | showTime rst = do dist <- getDistribution
                      return (getTimeCmdForDist dist ++ cmd)
  | otherwise       = return cmd
 where
  -- Time command for specific distributions. It might be necessary
  -- to adapt this command.
  getTimeCmdForDist dist
    | True --"Ubuntu" `isInfixOf` dist
    = "time --format=\""++timename++" time: %Us / elapsed: %E\" "
    | "Debian" `isInfixOf` dist
    = "export TIMEFORMAT=\""++timename++" time: %2Us / elapsed: %2Es\" && time "
    | otherwise = "time "

  getDistribution = do
    (hin, hout, herr) <- execCmd "lsb_release -i"
    dist <- hGetContents hout
    hClose hin
    hClose hout
    hClose herr
    return dist
