--- --------------------------------------------------------------------------
--- Benchmarking tool
---
--- This program defines the execution of all benchmarks and summarizes
--- their results.
---
--- @author  Michael Hanus, Björn Peemöller, Fabian Reck
--- @version January 2012
--- --------------------------------------------------------------------------

import Char
import IO
import IOExts
import List (isPrefixOf, isInfixOf, intersperse, last)
import Maybe
import System
import Time
import ReadShowTerm
import Float

-- ---------------------------------------------------------------------------
-- Flags
-- ---------------------------------------------------------------------------

-- Show benchmarks commands, like compiler calls, runtime calls,...?
doTrace = False

-- home directory of KiCS2:
kics2Home = "../.."

-- Set whether only KiCS2 benchmarks should be executed:
onlyKiCS2 = True

-- home directory of the monadic curry compiler
monHome      = "$HOME/.cabal/bin"
monlib       = "$HOME/.cabal/share/curry2monad-0.1"
monInstalled = False -- is the monadic curry compiler installed?

-- ---------------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------------

init :: [a] -> [a]
init []       = []
init [_]      = []
init (x:y:zs) = x : init (y : zs)

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls =  q : (case ls of
  []   -> []
  x:xs -> scanl f (f q x) xs)

liftIO :: (a -> b) -> IO a -> IO b
liftIO f act = act >>= return . f

forIO :: [a] -> (a -> IO b) -> IO [b]
forIO xs f = mapIO f xs

unless :: Bool -> IO () -> IO ()
unless p act = if p then done else act

when :: Bool -> IO () -> IO ()
when p act = if p then act else done

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

flushStrLn :: String -> IO ()
flushStrLn s = putStrLn s >> hFlush stdout

trace :: String -> IO ()
trace s = when doTrace $ flushStrLn s

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

type Command = (String, [String]) -- a shell command with arguments

showCmd :: Command -> String
showCmd (bin, args) = unwords $ bin : args

--- Run the command and returns its output from stdout
runCmd :: Command -> IO String
runCmd cmd = do
  h <- connectToCommand $ showCmd cmd
  s <- hGetContents h
  hClose h
  return s

--- Silently execute a command
sysCmd :: Command -> IO ()
sysCmd cmd = system (showCmd cmd) >> done

traceCmd :: Command -> IO ()
traceCmd cmd = trace call >> system call >> done
  where call = showCmd cmd

isUbuntu :: IO Bool
isUbuntu = isInfixOf "Ubuntu" `liftIO` runCmd ("lsb_release", ["-i"])

getHostName :: IO String
getHostName = runCmd ("uname", ["-n"])

getSystemInfo :: IO String
getSystemInfo = runCmd ("uname", ["-a"])

type TimeInfo =
  { tiCommand       :: String -- Command being timed
  , tiUserTime      :: Float  -- User time (seconds)
  , tiSystemTime    :: Float  -- System time (seconds)
  , tiPercentCPU    :: Int    -- Percent of CPU this job got
  , tiElapsedTime   :: Float  -- Elapsed (wall clock) time (h:mm:ss or m:ss)
  , tiSharedMem     :: Int    -- Average shared text size (kbytes)
  , tiUnsharedMem   :: Int    -- Average unshared data size (kbytes)
  , tiAvgStack      :: Int    -- Average stack size (kbytes)
  , tiAvgTotal      :: Int    -- Average total size (kbytes)
  , tiMaxResident   :: Int    -- Maximum resident set size (kbytes)
  , tiAvgResident   :: Int    -- Average resident set size (kbytes)
  , tiMajorFaults   :: Int    -- Major (requiring I/O) page faults
  , tiMinorFaults   :: Int    -- Minor (reclaiming a frame) page faults
  , tiVolSwitch     :: Int    -- Voluntary context switches
  , tiNonvolSwitch  :: Int    -- Involuntary context switches
  , tiSwaps         :: Int    -- Swaps
  , tiFSInputs      :: Int    -- File system inputs
  , tiFSOutputs     :: Int    -- File system outputs
  , tiSocketMsgSent :: Int    -- Socket messages sent
  , tiSocketMsgRecv :: Int    -- Socket messages received
  , tiSignalsDelivd :: Int    -- Signals delivered
  , tiPageSize      :: Int    -- Page size (bytes)
  , tiExitStatus    :: Int    -- Exit status
  }

toInfo :: [String] -> TimeInfo
toInfo [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23]
  = { tiCommand       = readQTerm   x1, tiUserTime      = readQTerm x2
    , tiSystemTime    = readQTerm   x3, tiPercentCPU    = readQTerm $ init x4
    , tiElapsedTime   = readElapsed x5, tiSharedMem     = readQTerm x6
    , tiUnsharedMem   = readQTerm   x7, tiAvgStack      = readQTerm x8
    , tiAvgTotal      = readQTerm   x9, tiMaxResident   = readQTerm x10
    , tiAvgResident   = readQTerm  x11, tiMajorFaults   = readQTerm x12
    , tiMinorFaults   = readQTerm  x13, tiVolSwitch     = readQTerm x14
    , tiNonvolSwitch  = readQTerm  x15, tiSwaps         = readQTerm x16
    , tiFSInputs      = readQTerm  x17, tiFSOutputs     = readQTerm x18
    , tiSocketMsgSent = readQTerm  x19, tiSocketMsgRecv = readQTerm x20
    , tiSignalsDelivd = readQTerm  x21, tiPageSize      = readQTerm x22
    , tiExitStatus    = readQTerm  x23  }
  where
    readElapsed hms = hh *. 3600 +. mm *. 60 +. ss
      where hh = if noHours then 0.0 else readQTerm p1
            mm = readQTerm $ if noHours then p1 else p2
            ss = readQTerm $ if noHours then p2 else (tail r2)
            noHours = null r2
            (p1,r1) = break (==':') hms
            (p2,r2) = break (==':') (tail r1)

--- Time the execution of a command and return
---   * the exit code
---   * the content written to stdout
---   * the content written to stderr
---   * the information gathered by the time command
timeCmd :: Command -> IO (Int, String, String, TimeInfo)
timeCmd (cmd, args) = do
  -- create timing process and execute it
  (exitCode, outCnts, errCnts) <- evalCmd timeCommand timeArgs []
  -- extract timing information
  timingInfo <- extractInfo `liftIO` readFile timeFile
  -- remove time file
  sysCmd ("rm", ["-rf", timeFile])
  return (exitCode, outCnts, errCnts, toInfo $ map snd timingInfo)
 where
  timeFile    = ".time"
  timeCommand = "/usr/bin/time"
  timeArgs    = [ "--quiet", "--verbose", "-o", timeFile ] ++ cmd : args
  extractInfo = map (splitInfo . trim) . lines
  trim        = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  splitInfo s@[]       = ([], s)
  splitInfo s@[_]      = ([], s)
  splitInfo s@(c:d:cs)
    | take 2 s == ": " = ([], cs)
    | otherwise        = let (k, v) = splitInfo (d:cs) in (c:k, v)

--- Execute a shell command and return the time of its execution
benchCmd :: Command -> IO (Bool, Float, Int)
benchCmd cmd = do
  (exitcode, outcnt, errcnt, ti) <- timeCmd cmd
  trace outcnt
  trace errcnt
  return $ (exitcode == 0, ti -> tiUserTime, ti -> tiMaxResident)

-- ---------------------------------------------------------------------------
-- Operations for running benchmarks.
-- ---------------------------------------------------------------------------

-- Each benchmark consists of a name, an action to prepare the benchmark
-- (e.g., compile the program), a command to run the benchmark
-- and a command to clean up all auxiliary files at the end of a benchmark
type Benchmark =
  { bmName    :: String
  , bmPrepare :: IO Int
  , bmCommand :: Command
  , bmCleanup :: Command
  }

-- Run a benchmark and return its timings
runBenchmark :: Int -> Int -> (Int, Benchmark) -> IO (String, [Float], [Int])
runBenchmark rpts totalNum (currentNum, benchMark) = do
  flushStr $ "Running benchmark [" ++ show currentNum ++ " of "
             ++ show totalNum ++ "]: " ++ (benchMark -> bmName) ++ ": "
  benchMark -> bmPrepare
  infos <- sequenceIO $ replicate rpts $ benchCmd $ benchMark -> bmCommand
  sysCmd $ benchMark -> bmCleanup
  let (successful, times, mems) = unzip3 infos
  flushStrLn $ if and successful then "PASSED" else "FAILED"
  trace $ "RUNTIMES: " ++ intercalate " | " (map show times)
  trace $ "MEMUSAGE: " ++ intercalate " | " (map show mems)
  return (benchMark -> bmName, times, mems)

-- Run a set of benchmarks and return the timings
runBenchmarks :: Int -> Int -> (Int, [Benchmark]) -> IO String
runBenchmarks rpts total (start, benchmarks) = do
  results <- mapIO (runBenchmark rpts total) (zip [start ..] benchmarks)
  let maxnamelength = foldr max 0 (map (length . fst3) results)
  return $ unlines $
    replicate 8 '-' :
    map (\ (n,ts,ms) -> rpad maxnamelength n
        ++ "|" ++ intercalate "|" (map showFloat ts)
        ++ " || " ++ intercalate " | " (map show ms)
        )
        (if all (not . null) (map snd3 results)
         then processResults results
         else results)
 where
  fst3 (x,_,_) = x
  snd3 (_,x,_) = x
  rpad n str = str ++ replicate (n - length str) ' '
  lpad n str = replicate (n - length str) ' ' ++ str
  showTimeMem (t, m) = lpad 5 (showFloat t) ++ '@' : lpad 10 (show m)
  showFloat x = let (x1,x2) = break (=='.') (show x)
                 in take (3-length x1) (repeat ' ') ++ x1 ++ x2 ++
                    take (3-length x2) (repeat '0') ++ " "

  processResults results =
    let (names,times,mems) = unzip3 results
    in  zip3 names (processTimes times) mems

  processTimes timings =
    let means        = map mean timings
        roundedmeans = map truncateFloat means
        mintime      = foldr1 min means
        minNonZero   = if mintime == 0.0 then 0.0001 else mintime
        normalized   = map (truncateFloat . (/.minNonZero)) means
    in  zipWith (:) normalized (if length (head timings) == 1
                                then timings
                                else zipWith (:) roundedmeans timings)
   where
    mean :: [Float] -> Float
    mean xs = (foldr1 (+.) xs) /. (i2f (length xs))

    truncateFloat x = i2f (round (x*.100)) /. 100

-- Run all benchmarks and show results
run :: Int -> [[Benchmark]] -> IO ()
run rpts benchmarks = do
  args <- getArgs
  let total = length (concat benchmarks)
      startnums = scanl (+) 1 $ map length benchmarks
  results <- mapIO (runBenchmarks rpts total) (zip startnums benchmarks)
  ltime <- getLocalTime
  info  <- getSystemInfo
  mach  <- getHostName
  let res = "Benchmarks on system " ++ info ++ "\n" ++
            "Format of timings: normalized|mean|runtimes...\n\n" ++
            concat results
  putStrLn res
  unless (null args) $ writeFile (outputFile (head args) (init mach) ltime) res

outputFile :: String -> String -> CalendarTime -> String
outputFile name mach (CalendarTime ye mo da ho mi se _) = "../results/"
  ++ name ++ '@' : mach
  ++ intercalate "_" (map show [ye, mo, da, ho, mi, se])
  ++ ".bench"

-- ---------------------------------------------------------------------------
-- Benchmarks for various systems
-- ---------------------------------------------------------------------------

data Supply   = S_PureIO | S_IORef | S_GHC | S_Integer

data Strategy
  = PRDFS -- primitive
  | IODFS    | IOBFS    | IOIDS Int String    | IOIDS2 Int String -- top-level
  | MPLUSDFS | MPLUSBFS | MPLUSIDS Int String | MPLUSPar          -- MonadPlus
  | EncDFS   | EncBFS   | EncIDS                                  -- encapsulated

data Goal   = Goal Bool String String -- non-det? / main-expr / module
data Output = All | One | Interactive | Count

detGoal :: String -> String -> Goal
detGoal gl mod = Goal False mod gl

nonDetGoal :: String -> String -> Goal
nonDetGoal gl mod = Goal True mod gl

showSupply :: Supply -> String
showSupply = map toUpper . drop 2 . show

chooseSupply :: Supply -> String
chooseSupply = map toLower . drop 2 . show

mainExpr :: Strategy -> Output -> Goal -> String
mainExpr _ _ (Goal False _ goal) = "evalD d_C_" ++ goal
mainExpr s o (Goal True  _ goal) = searchExpr s
 where
  searchExpr PRDFS            = "main = prdfs print nd_C_" ++ goal
  searchExpr IODFS            = searchComb "ioDFS"
  searchExpr IOBFS            = searchComb "ioBFS"
  searchExpr (IOIDS    i inc) = searchComb $ "(ioIDS " ++ show i ++ " " ++ inc ++ ")"
  searchExpr (IOIDS2   i inc) = searchComb $ "(ioIDS2 " ++ show i ++ " " ++ inc ++ ")"
  searchExpr MPLUSDFS         = searchComb "mplusDFS"
  searchExpr MPLUSBFS         = searchComb "mplusBFS"
  searchExpr (MPLUSIDS i inc) = searchComb $ "(mplusIDS " ++ show i ++ " " ++ inc ++ ")"
  searchExpr MPLUSPar         = searchComb "mplusPar"
  searchExpr EncDFS           = wrapEnc "DFS"
  searchExpr EncBFS           = wrapEnc "BFS"
  searchExpr EncIDS           = wrapEnc "IDS"
  wrapEnc strat      = "import qualified Curry_SearchTree as ST\n"
    ++ "main = prdfs print (\\i c -> ST.d_C_allValues" ++ strat
    ++ " (ST.d_C_someSearchTree (nd_C_" ++ goal ++ " i c) c) c)"
  searchComb search  = "main = " ++ comb ++ " " ++ search ++ " $ " ++ "nd_C_" ++ goal
  comb = case o of
    All         -> "printAll"
    One         -> "printOne"
    Interactive -> "printInteractive"
    Count       -> "countAll"

kics2 hoOpt ghcOpt supply strategy output gl@(Goal _ mod goal)
  = idcBenchmark tag mod hoOpt ghcOpt (chooseSupply supply) (mainExpr strategy output gl)
 where tag = concat [ "IDC"
                    , if ghcOpt then "+"  else ""
                    , if hoOpt  then "_D" else ""
                    , '_' : show strategy
                    , '_' : showSupply supply
                    , if goal == "main" then "" else ':':goal
                    ]

monc (Goal _ mod goal) = monBenchmark "MON+" mod True goal

pakcs (Goal _ mod goal)
  | goal == "main" = pakcsBenchmark "" mod
  | otherwise      = pakcsBenchmark ("-m \"print " ++ goal ++ "\"") mod

mcc (Goal _ mod goal)
  | goal == "main" = mccBenchmark "" mod
  | otherwise      = mccBenchmark  ("-e\"" ++ goal ++ "\"") mod

ghc  (Goal False mod _) = ghcBenchmark mod
ghc  (Goal True  _   _) = []
ghcO (Goal False mod _) = ghcOBenchmark mod
ghcO (Goal True  _   _) = []

sicstus (Goal _ mod _) = sicsBenchmark mod
swipl   (Goal _ mod _) = swiBenchmark mod

skip _ = []

idcBenchmark tag mod hooptim ghcoptim idsupply mainexp =
  [ { bmName    = mod ++ "@" ++ tag
    , bmPrepare = idcCompile mod hooptim ghcoptim idsupply mainexp
    , bmCommand = ("./Main", [])
    , bmCleanup = ("rm", ["-f", "Main*"]) -- , ".curry/" ++ mod ++ ".*", ".curry/kics2/Curry_*"])
    }
  ]
monBenchmark tag mod optim mainexp = if monInstalled && not onlyKiCS2
  then [ { bmName    = mod ++ "@" ++ tag
         , bmPrepare = monCompile mod optim mainexp
         , bmCommand = ("./Main", [])
         , bmCleanup = ("rm", ["-f", "Main*", "Curry_*"])
         }
       ]
  else []
pakcsBenchmark options mod = if onlyKiCS2 then [] else
  [ { bmName    = mod ++ "@PAKCS "
    , bmPrepare = pakcsCompile options mod
    , bmCommand = ("./" ++ mod ++ ".state", [])
    , bmCleanup = ("rm", ["-f", mod ++ ".state"])
    }
  ]
mccBenchmark options mod = if onlyKiCS2 then [] else
  [ { bmName    = mod ++ "@MCC   "
    , bmPrepare = mccCompile options mod
    , bmCommand = ("./a.out +RTS -h512m -RTS", [])
    , bmCleanup = ("rm", ["-f", "a.out", mod ++ ".icurry"])
    }
  ]
ghcBenchmark mod = if onlyKiCS2 then [] else
  [ { bmName    = mod ++ "@GHC   "
    , bmPrepare = ghcCompile mod
    , bmCommand = ("./" ++ mod, [])
    , bmCleanup = ("rm", ["-f", mod, mod ++ ".hi", mod ++ ".o"])
    }
  ]
ghcOBenchmark mod = if onlyKiCS2 then [] else
  [ { bmName    = mod ++ "@GHC+  "
    , bmPrepare = ghcCompileO mod
    , bmCommand = ("./" ++ mod, [])
    , bmCleanup = ("rm", ["-f", mod, mod ++ ".hi", mod ++ ".o"])
    }
  ]
sicsBenchmark mod = if onlyKiCS2 then [] else
  [ { bmName    = mod ++ "@SICS  "
    , bmPrepare = sicstusCompile src
    , bmCommand = ("./" ++ src ++ ".state", [])
    , bmCleanup = ("rm", ["-f", src ++ ".state"])
    }
  ] where src = map toLower mod
swiBenchmark mod = if onlyKiCS2 then [] else
  [ { bmName    = mod ++ "@SWI   "
    , bmPrepare = swiCompile src
    , bmCommand = ("./" ++ src ++ ".state", [])
    , bmCleanup = ("rm", ["-f", src ++ ".state"])
    }
  ] where src = map toLower mod

-- ---------------------------------------------------------------------------
-- Compile target with KiCS2
-- ---------------------------------------------------------------------------

-- Command to compile a module and execute main with idcompiler:
-- arg1: module name
-- arg2: compile with higher-order optimization?
-- arg3: compile Haskell target with GHC optimization?
-- arg4: idsupply implementation (integer or pureio)
-- arg5: main (Haskell!) call
idcCompile mod hooptim ghcoptim idsupply mainexp = do
  -- 1. Call the idc to create the Haskell module
  let idcCmd = (kics2Home ++ "/bin/idc", [ "-q", if hooptim then "" else "-O0"
                                         , "-i" ++ kics2Home ++ "/lib", mod])
  traceCmd idcCmd

  -- 2. Create the Main.hs program containing the call to the initial expression
  let mainFile = "Main.hs"
  let mainCode = unlines  [ "module Main where"
                          , "import Basics", "import Curry_" ++ mod
                          , mainexp
                          ]
  -- show to put parentheses around the source code
  let mainCmd = ("echo", [show mainCode, ">", mainFile])
  traceCmd mainCmd

  -- 3. Call the GHC
  let ghcImports = [ kics2Home ++ "/runtime"
                   , kics2Home ++ "/runtime/idsupply" ++ idsupply
                   ,".curry/kics2"
                   , kics2Home ++ "/lib/.curry/kics2"
                   ]
      ghcCmd = ("ghc" , [ if ghcoptim then "-O2" else ""
                        , "--make"
                        , if doTrace then "" else "-v0"
                        , "-package ghc"
                        , "-cpp" -- use the C preprocessor
                        , "-DDISABLE_CS" -- disable constraint store
                        --,"-DSTRICT_VAL_BIND" -- strict value bindings
                        , "-XMultiParamTypeClasses","-XFlexibleInstances"
--                         , "-fforce-recomp"
                        , "-i" ++ intercalate ":" ghcImports
                        , mainFile
                        ]) -- also:  -funbox-strict-fields ?
  traceCmd ghcCmd
  return 0

-- ---------------------------------------------------------------------------
-- Compile target with Monadic Curry
-- ---------------------------------------------------------------------------

-- Command to compile a module and execute main with monadic curry:
-- arg1: module name
-- arg2: compile with optimization?
-- arg3: main (Curry!) call
monCompile mod optim mainexp = do
  let c2mCmd = monHome ++ "/curry2monad -m" ++ mainexp ++ " " ++ mod
  putStrLn $ "Executing: " ++ c2mCmd
  system c2mCmd

  let haskellMain = "cM_" ++ mainexp
  writeFile "Main.hs" $ unlines
    [ "module Main where"
    , "import Curry_" ++ mod
    , "main = print $ " ++ haskellMain
    ]
  putStrLn $ "Main expression: " ++ haskellMain
  let imports    = [monlib]
      compileCmd = unwords ["ghc",if optim then "-O2" else "","--make",
                            "-fforce-recomp",
                            "-i"++intercalate ":" imports,"Main.hs"]
  putStrLn $ "Executing: "++ compileCmd
  system compileCmd

-- Command to compile a module and execute main with MCC:
--mccCompile mod = "/home/mcc/bin/cyc -e\"print main\" " ++ mod ++".curry"
mccCompile options mod = system $ "/home/mcc/bin/cyc " ++
    (if null options then "-e\"main\"" else options) ++
    " " ++ mod ++".curry"

-- Command to compile a module and execute main with GHC:
ghcCompile mod = system $ "ghc --make -fforce-recomp " ++ mod

-- Command to compile a module and execute main with GHC (optimized):
ghcCompileO mod = system $ "ghc -O2 --make -fforce-recomp " ++ mod

-- Command to compile a module and print main in PAKCS:
pakcsCompile options mod = system $ "/home/pakcs/pakcs/bin/pakcs "++
    (if null options then "-m \"print main\"" else options) ++" -s  " ++ mod

-- Command to compile a Prolog program and run main in SICStus-Prolog:
sicstusCompile mod = system $ "echo \"compile("++mod++"), save_program('"++mod++".state',main).\" | /home/sicstus/sicstus4/bin/sicstus && chmod +x "++mod++".state"

-- Command to compile a Prolog program and run main in SWI-Prolog:
swiCompile mod = system $ "echo \"compile("++mod++"), qsave_program('"++mod++".state',[toplevel(main)]).\" | /home/swiprolog/bin/swipl"

-- ---------------------------------------------------------------------------
-- The various sets of systems
-- ---------------------------------------------------------------------------

-- Benchmarking functional programs with kics2/pakcs/mcc/ghc/sicstus/swi
benchFOFP :: Bool -> Goal -> [Benchmark]
benchFOFP withMon goal = concatMap ($goal)
  [ kics2 True False S_Integer PRDFS All
  , kics2 True True  S_Integer PRDFS All
  , pakcs
  , mcc
  , ghc
  , ghcO
  , sicstus
  , swipl
  , if withMon then monc else skip
  ]

-- Benchmarking higher-order functional programs with idc/pakcs/mcc/ghc
benchHOFP :: Bool -> Goal -> [Benchmark]
benchHOFP withMon goal = concatMap ($goal)
  [ kics2 True False S_Integer PRDFS All
  , kics2 True True  S_Integer PRDFS All
  , pakcs
  , mcc
  , ghc
  , ghcO
  , if withMon then monc else skip
  ]

-- Benchmarking functional logic programs with idc/pakcs/mcc in DFS mode
benchFLPDFS :: Bool -> Goal -> [Benchmark]
benchFLPDFS withMon goal = concatMap ($goal)
  [ kics2 True False S_Integer PRDFS All
  , kics2 True True  S_Integer PRDFS All
  , kics2 True True  S_PureIO  PRDFS All
  , pakcs
  , mcc
  , if withMon then monc else skip
  ]

-- Benchmarking functional logic programs with unification with idc/pakcs/mcc
benchFLPDFSU :: Goal -> [Benchmark]
benchFLPDFSU goal = concatMap ($goal)
  [ kics2 True True S_PureIO PRDFS All
  , kics2 True True S_PureIO IODFS All
  , pakcs
  , mcc
  ]

-- Benchmarking functional patterns with idc/pakcs
benchFunPats :: Goal -> [Benchmark]
benchFunPats goal = concatMap ($goal)
  [ kics2 True True S_PureIO PRDFS All
  , kics2 True True S_PureIO IODFS All
  , pakcs
  ]

-- Benchmarking functional programs with idc/pakcs/mcc
-- with a given name for the main operation
benchFPWithMain :: Goal -> [Benchmark]
benchFPWithMain goal = concatMap ($goal)
  [ kics2 True True S_Integer IODFS All, pakcs, mcc ]

-- Benchmarking functional logic programs with idc/pakcs/mcc in DFS mode
-- with a given name for the main operation
benchFLPDFSWithMain :: Goal -> [Benchmark]
benchFLPDFSWithMain goal = concatMap ($goal)
  [ kics2 True False S_Integer PRDFS All
  , kics2 True True  S_Integer PRDFS All
  , kics2 True True  S_PureIO  PRDFS All
  , pakcs
  , mcc
  ]

-- Benchmarking functional logic programs with different id supply and DFS:
benchIDSupply :: Goal -> [Benchmark]
benchIDSupply goal = concatMap
  (\su -> kics2 True True su IODFS All goal)
  [S_PureIO, S_IORef, S_GHC, S_Integer]

-- Benchmarking functional logic programs with different search strategies
benchFLPSearch :: Goal -> [Benchmark]
benchFLPSearch prog = concatMap (\st -> kics2 True True S_IORef st All prog)
  [ PRDFS, IODFS, IOIDS 10 "(+1)", IOIDS 10 "(*2)", IOIDS2 10 "(+1)", IOIDS2 10 "(*2)" -- , IOBFS is too slow
  , MPLUSDFS, MPLUSBFS, MPLUSIDS 10 "(+1)", MPLUSIDS 10 "(*2)", MPLUSPar]

-- Benchmarking functional logic programs with different search strategies
-- extracting only the first result
benchFLPFirst :: Goal -> [Benchmark]
benchFLPFirst prog = concatMap (\st -> kics2 True True S_IORef st One prog)
  [ PRDFS, IODFS, IOIDS 10 "(+1)", IOIDS 10 "(*2)", IOIDS2 10 "(+1)", IOIDS2 10 "(*2)" -- , IOBFS is too slow
  , MPLUSDFS, MPLUSBFS, MPLUSIDS 10 "(+1)", MPLUSIDS 10 "(*2)", MPLUSPar]


-- Benchmarking FL programs that require complete search strategy
benchFLPCompleteSearch :: Goal -> [Benchmark]
benchFLPCompleteSearch goal = concatMap
  (\st -> kics2 True True S_IORef st One goal)
  [IOBFS, IOIDS 100 "(*2)"]

-- Benchmarking functional logic programs with different search strategies
-- for "main" operations and goals for encapsulated search strategies
benchFLPEncapsSearch :: Goal -> [Benchmark]
benchFLPEncapsSearch goal = concatMap
  (\st -> kics2 True True S_IORef st All goal)
  [IODFS, IOBFS, IOIDS 100 "(*2)", EncDFS, EncBFS, EncIDS]

-- Benchmarking =:<=, =:= and ==
benchFLPDFSKiCS2WithMain :: Bool -> Bool -> Goal -> [Benchmark]
benchFLPDFSKiCS2WithMain withPakcs withMcc goal = concatMap ($goal)
  [ kics2 True True S_PureIO PRDFS All
  , kics2 True True S_PureIO IODFS All
  , if withPakcs then pakcs else skip
  , if withMcc   then mcc   else skip
  ]

-- Benchmarking FL programs that require complete search strategy
benchIDSSearch :: Goal -> [Benchmark]
benchIDSSearch prog = concatMap
  (\st -> kics2 True True S_IORef st Count prog)
  [IOIDS 100 "(*2)", IOIDS 100 "(+1)", IOIDS2 100 "(+1)"]

-- ---------------------------------------------------------------------------
-- goal collections
-- ---------------------------------------------------------------------------

-- first-order functional programming
fofpGoals :: [Goal]
fofpGoals = map (detGoal "main")
  [ "ReverseUser", "Reverse", "Tak", "TakPeano" ]

-- higher-order functional programming
hofpGoals :: [Goal]
hofpGoals = map (detGoal "main")
  [ "ReverseHO", "ReverseBuiltin", "Primes", "PrimesPeano"
  , "PrimesBuiltin", "Queens", "QueensUser"
  ]

-- functional programming
fpGoals :: [Goal]
fpGoals = fofpGoals ++ hofpGoals

searchGoals :: [Goal]
searchGoals = map (nonDetGoal "main")
  [ "SearchEmbed" , "SearchGraph" , "SearchHorseman"
  , "SearchMAC"   , "SearchQueens" -- , "SearchSMM" -- too slow
  , "PermSort"    , "Last"        , "Half"
  ]
  -- "SearchCircuit" : needs CLPR
  -- "SearchLakritz" : needs CLPFD
  -- "SearchZebra"   : needs CLPFD


allBenchmarks = concat
  [ map (benchFOFP   True) fofpGoals
  , map (benchHOFP   False    . detGoal    "main") [ "ReverseBuiltin"]
  , map (benchHOFP   True     . detGoal    "main") [ "ReverseHO", "Primes", "PrimesPeano", "PrimesBuiltin", "Queens", "QueensUser" ]
  , map (benchFLPDFS True     . nonDetGoal "main") ["PermSort", "PermSortPeano" ]
  , map (benchFLPDFS False    . nonDetGoal "main") ["Half"]
  , map (benchFLPSearch       . nonDetGoal "main") ["PermSort", "PermSortPeano", "Half"]
  , [benchFLPCompleteSearch   $ nonDetGoal "main"  "NDNums"]
  , [benchFPWithMain          $ detGoal    "goal1" "ShareNonDet"]
  , [benchFLPDFSWithMain      $ nonDetGoal "goal2" "ShareNonDet"]
  , [benchFLPDFSWithMain      $ nonDetGoal "goal3" "ShareNonDet"]
  , map (benchFLPDFSU         . nonDetGoal "main") ["Last", "RegExp"]
  , map (benchIDSupply        . nonDetGoal "main") ["PermSort", "Half", "Last", "RegExp"]
  , map (benchFunPats         . nonDetGoal "main") ["LastFunPats", "ExpVarFunPats", "ExpSimpFunPats", "PaliFunPats"]
  , map (benchFLPEncapsSearch . nonDetGoal "main") ["Half", "Last", "PermSort"]
  ]

unif =
     [
       -- mcc does not support function pattern
--       benchFLPDFSKiCS2WithMain "UnificationBenchFunPat" "goal_last_1L" True False
       -- mcc does not support function pattern
       benchFLPDFSKiCS2WithMain True False $ nonDetGoal "UnificationBenchFunPat" "goal_last_2L"
     , benchFLPDFSKiCS2WithMain True True  $ nonDetGoal "UnificationBench" "goal_last_2S"
       -- pakcs and mcc suspend on this goal
     , benchFLPDFSKiCS2WithMain False False $ nonDetGoal "UnificationBench" "goal_last_2Eq"
--     , benchFLPDFSKiCS2WithMain "UnificationBench" "goal_grep_S" True True
       -- pakcs and mcc suspend on this goal
--     , benchFLPDFSKiCS2WithMain "UnificationBench" "goal_grep_Eq" False False
       -- mcc does not support function pattern, pakcs runs very long (\infty?)
--     , benchFLPDFSKiCS2WithMain "UnificationBenchFunPat" "goal_half_L" False False
--     , benchFLPDFSKiCS2WithMain "UnificationBench" "goal_half_S" True True
--     , benchFLPDFSKiCS2WithMain "UnificationBench" "goal_half_Eq" True True
       -- mcc does not support function pattern
     , benchFLPDFSKiCS2WithMain True False  $ nonDetGoal "UnificationBenchFunPat" "goal_expVar_L"
     , benchFLPDFSKiCS2WithMain True True   $ nonDetGoal "UnificationBench" "goal_expVar_S"
     , benchFLPDFSKiCS2WithMain False False $ nonDetGoal "UnificationBench" "goal_expVar_Eq"
       -- mcc does not support function pattern
     , benchFLPDFSKiCS2WithMain True False $ nonDetGoal "UnificationBenchFunPat" "goal_expVar_L'"
     , benchFLPDFSKiCS2WithMain True True  $ nonDetGoal "UnificationBench" "goal_expVar_S'"
       -- pakcs and mcc suspend on this goal
     , benchFLPDFSKiCS2WithMain False False $ nonDetGoal "UnificationBench" "goal_expVar_Eq'"
       -- mcc does not support function pattern
     , benchFLPDFSKiCS2WithMain True False $ nonDetGoal "UnificationBenchFunPat" "goal_expVar_L''"
     , benchFLPDFSKiCS2WithMain True True  $ nonDetGoal "UnificationBench" "goal_expVar_S''"
       -- pakcs and mcc suspend on this goal
     , benchFLPDFSKiCS2WithMain False False $ nonDetGoal "UnificationBench" "goal_expVar_Eq''"
       -- mcc does not support function pattern
--     , benchFLPDFSKiCS2WithMain "UnificationBenchFunPat" "goal_simplify_L" True False
--     , benchFLPDFSKiCS2WithMain "UnificationBench" "goal_simplify_S" True True
       -- pakcs and mcc suspend on this goal
--     , benchFLPDFSKiCS2WithMain "UnificationBench" "goal_simplify_Eq" False False
       -- mcc does not support function pattern, pakcs runs very long (\infty?)
--     , benchFLPDFSKiCS2WithMain "UnificationBenchFunPat" "goal_pali_L" False False
--     , benchFLPDFSKiCS2WithMain "UnificationBench" "goal_pali_S" True True
       -- pakcs and mcc suspend on this goal
--     , benchFLPDFSKiCS2WithMain "UnificationBench" "goal_pali_Eq" False False
--     , benchFLPDFSKiCS2WithMain "UnificationBench" "goal_horseMan_S" True True
       -- pakcs and mcc suspend on this goal
--     , benchFLPDFSKiCS2WithMain "UnificationBench" "goal_horseMan_Eq" False False
     ]

benchSearch = map benchFLPSearch searchGoals
           ++ map benchFLPFirst (searchGoals ++ [nonDetGoal "main2" "NDNums"])

main = run 2 benchSearch
--main = run 1 allBenchmarks
--main = run 3 allBenchmarks
--main = run 1 [benchFLPCompleteSearch "NDNums"]
--main = run 1 (benchFPWithMain "ShareNonDet" "goal1" : [])
--           map (\g -> benchFLPDFSWithMain "ShareNonDet" g) ["goal2","goal3"])
--main = run 1 [benchHOFP "Primes" True]
--main = run 1 [benchFLPDFS "PermSort",benchFLPDFS "PermSortPeano"]
--main = run 1 [benchFLPSearch "PermSort",benchFLPSearch "PermSortPeano"]
--main = run 1 [benchFLPSearch "Half"]
--main = run 1 [benchFLPEncapsSearch "HalfSearchTree" ["encDFS","encBFS","encIDS"]]
--main = run 1
--  [benchFLPEncapsSearch "PermSortSearchTree" ["encDFS","encBFS","encIDS"]
--  ,benchFLPEncapsSearch "HalfSearchTree" ["encDFS","encBFS","encIDS"]
--  ,benchFLPEncapsSearch "LastSearchTree" ["encDFS","encBFS","encIDS"]]
--main = run 1 [benchFLPDFSU "Last"]
--main = run 1 [benchFLPDFSU "RegExp"]
--main = run 1 (map benchFunPats ["ExpVarFunPats","ExpSimpFunPats","PaliFunPats"
--main = run 3 unif
--main = run 1 [benchIDSupply "Last", benchIDSupply "RegExp"]

-- Evaluate log file of benchmark, i.e., compress it to show all results:
-- showLogFile :: IO ()
-- showLogFile = readFile "bench.log" >>= putStrLn . unlines . splitBMTime . lines
--  where
--   splitBMTime xs =
--     let (ys,zs) = break (\cs -> take 14 cs == "BENCHMARKTIME=") xs
--      in if null zs
--         then []
--         else drop (length ys - 2) ys ++ take 2 zs ++ ["------"] ++
--              splitBMTime (drop 2 zs)
