--- --------------------------------------------------------------------------
--- ID based curry compiler
---
--- @author  Bernd Brassel, Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version June 2011
--- --------------------------------------------------------------------------
module Compile where

import Prelude
import FiniteMap ( FM, addToFM, emptyFM, mapFM, filterFM, fmToList, listToFM
  , lookupFM, plusFM)
import Maybe (fromJust, fromMaybe, isJust)
import List (intersperse, find)
import Directory (doesFileExist)
import FileGoodies
import FlatCurry
import FlatCurryGoodies (funcName, consName, updQNamesInProg, isTypeSyn)
import ReadShowTerm (readQTermFile)

import qualified AbstractHaskell as AH
import qualified AbstractHaskellGoodies as AHG
import AbstractHaskellPrinter (showProg,showLiteral,showInt,showFloat)
import Analysis
import CompilerOpts
import Files
import FlatCurry2AbstractHaskell (fcy2abs)
import qualified FlatCurry2Types as FC2T (fcyTypes2abs)
import LiftCase (liftCases)
import Message (putErrLn, showStatus, showAnalysis, showDetail)
import ModuleDeps (ModuleIdent, Source, deps)
import Names
  ( renameModule, renameFile, renameQName, consPrefix, funcPrefix
  , mkChoiceName, mkChoicesName, mkGuardName
  , externalFunc, externalFile, destFile, analysisFile, funcInfoFile )
import SimpleMake (smake)
import Splits (mkSplits)
import Utils (foldIO, intercalate, unless, when)

-- parse the command-line arguments and build the specified files
main :: IO ()
main = do
  (opts, files) <- compilerOpts
  mapIO_ (build opts) files

-- Load the module, resolve the dependencies and compile the source files
-- if necessary
build :: Options -> String -> IO ()
build opts fn = do
  exists <- doesFileExist fn
  mbFn <- if exists
    then return (Just fn)
    else lookupFileInPath fn [".curry", ".lcurry"] ["."]
  case mbFn of
    Nothing -> putErrLn $ "Could not find file " ++ fn
    Just f -> do
      (mods, errs) <- deps opts f
      if null errs
        then foldIO (makeModule mods) initState (zip mods [1 .. ]) >> done
        else mapIO_ putErrLn errs
        where initState = { compOptions := opts | defaultState }

makeModule :: [(ModuleIdent, Source)] -> State -> ((ModuleIdent, Source), Int)
           -> IO State
makeModule mods state mod@((_, (fn, fcy)), _)
  | opts -> optForce  = compileModule modCnt state mod
  | otherwise         = do
                        depFiles <- getDepFiles
                        smake (destFile (opts -> optOutputSubdir) fn)
                              depFiles
                              (compileModule modCnt state mod)
                              (loadAnalysis modCnt state mod)
  where
    getDepFiles = do
      hasExternals <- doesFileExist extFile
      let ownModule = fn : if hasExternals then [extFile] else []
      let imported  = map (\i -> destFile (opts -> optOutputSubdir)
                               $ fst $ fromJust $ lookup i mods) imps
      return $ ownModule ++ imported
    extFile = externalFile fn
    (Prog _ imps _ _ _) = fcy
    modCnt = length mods
    opts = state -> compOptions

type AnalysisResult = (TypeMap, NDResult, HOResult, HOResult)

storeAnalysis :: State -> AnalysisResult -> String -> IO ()
storeAnalysis state (types, ndAna, hoFunAna, hoConsAna) fn = do
  showDetail opts $ "Writing Analysis file " ++ ndaFile
  writeQTermFileInDir ndaFile (ndAnaStr, hoFuncAnaStr, hoConsAnaStr, typesStr)
    where
      opts         = state -> compOptions
      ndaFile      = analysisFile (opts -> optOutputSubdir) fn
      ndAnaStr     = showMap ndAna
      hoFuncAnaStr = showMap hoFunAna
      hoConsAnaStr = showMap hoConsAna
      typesStr     = showMap types


loadAnalysis :: Int -> State -> ((ModuleIdent, Source), Int) -> IO State
loadAnalysis total state ((mid, (fn, _)), current) = do
  showStatus opts $ compMessage current total ("Analyzing " ++ mid) fn ndaFile
  (ndAnalysis, hoFuncAnalysis, hoConsAnalysis, types) <- readQTermFile ndaFile
  return { ndResult     := (state -> ndResult    ) `plusFM` readMap ndAnalysis
         , hoResultFun  := (state -> hoResultFun ) `plusFM` readMap hoFuncAnalysis
         , hoResultCons := (state -> hoResultCons) `plusFM` readMap hoConsAnalysis
         , typeMap      := (state -> typeMap     ) `plusFM` readMap types
         | state }
    where
      ndaFile = analysisFile (opts -> optOutputSubdir) fn
      opts = state -> compOptions

compileModule :: Int -> State -> ((ModuleIdent, Source), Int) -> IO State
compileModule total state ((mid, (fn, fcy)), current) = do
  showStatus opts $ compMessage current total ("Compiling " ++ mid) fn destination

  let fcy' = filterPrelude opts fcy
  dump DumpFlat opts fcyName (show fcy')

  showDetail opts "Lifting case expressions"
  let pLifted = liftCases True fcy'
  dump DumpLifted opts liftedName (show pLifted)

  showDetail opts "Renaming symbols"
  let renamed@(Prog _ _ ts _ _)  = rename pLifted
  dump DumpRenamed opts renamedName (show renamed)

  showDetail opts "Transforming functions"
  ((tProg,modAnalysisResult), state') <- unM (transProg renamed) state
  storeAnalysis state' modAnalysisResult fn
  let ahsFun@(AH.Prog n imps _ funs ops) = fcy2abs tProg
  dump DumpFunDecls opts funDeclName (show ahsFun)

  showDetail opts "Transforming type declarations"
  let typeDecls = FC2T.fcyTypes2abs (state' -> hoResultCons) ts
  dump DumpTypeDecls opts typeDeclName (show typeDecls)

  showDetail opts "Combining to Abstract Haskell"
  let ahs = (AH.Prog n (defaultModules ++ imps) typeDecls funs ops)

  -- TODO: HACK: manually patch export of type class curry into Prelude
  let ahsPatched = patchCurryTypeClassIntoPrelude ahs
  dump DumpAbstractHs opts abstractHsName (show ahsPatched)

  showDetail opts "Integrating external declarations"
  integrated <- integrateExternals opts ahsPatched fn

  showDetail opts $ "Generating Haskell module " ++ destination
  writeFileInDir destination integrated

  showDetail opts $ "Writing auxiliary info file " ++ funcInfo
  writeQTermFileInDir funcInfo (extractFuncInfos funs)

  showDetail opts $ "Done"
  return state'

    where
    fcyName        = fcyFile $ withBaseName (++ "Dump")      mid
    liftedName     = fcyFile $ withBaseName (++ "Lifted")    mid
    renamedName    = fcyFile $ withBaseName (++ "Renamed")   mid
    funDeclName    = ahsFile $ withBaseName (++ "FunDecls")  mid
    typeDeclName   = ahsFile $ withBaseName (++ "TypeDecls") mid
    abstractHsName = ahsFile mid
    destination    = destFile (opts -> optOutputSubdir) fn
    funcInfo       = funcInfoFile (opts -> optOutputSubdir) fn
    opts           = state -> compOptions
    fcyFile f = withExtension (const ".fcy") f
    ahsFile f = withExtension (const ".ahs") f

-- Extract some basic information (deterministic, IO) about all functions
extractFuncInfos funs =
  map (\fd -> (AHG.funcName fd, isIO (AHG.typeOf fd))) funs
 where
  isIO AH.Untyped = False
  isIO (AH.FType texp) = withIOResult texp
  isIO (AH.CType _ texp) = withIOResult texp

  withIOResult (AH.TVar _) = False
  withIOResult (AH.FuncType _ texp) = withIOResult texp
  withIOResult (AH.TCons tc _) = tc == (curryPrelude, "C_IO")

-- Patch Prelude in order to add some exports for predefined items
patchCurryTypeClassIntoPrelude :: AH.Prog -> AH.Prog
patchCurryTypeClassIntoPrelude p@(AH.Prog m imps td fd od)
  | m == curryPrelude = AH.Prog m imps (curryDecl:td) fd od
  | otherwise         = p
 where
  curryDecl = AH.Type (curryPrelude, "Curry") AH.Public [] []

compMessage :: Int -> Int -> String -> String -> String -> String
compMessage curNum maxNum msg fn dest
  =  '[' : fill max sCurNum ++ " of " ++ sMaxNum  ++ "]"
  ++ ' ' : msg  ++ " ( " ++ fn ++ ", " ++ dest ++ " )"
    where
      sCurNum = show curNum
      sMaxNum = show maxNum
      max = length $ sMaxNum
      fill n s = replicate (n - length s) ' ' ++ s

filterPrelude :: Options -> Prog -> Prog
filterPrelude opts p@(Prog m imps td fd od)
  | noPrelude = Prog m (filter (/= prelude) imps) td fd od
  | otherwise = p
  where noPrelude = ExtNoImplicitPrelude `elem` opts -> optExtensions

--
integrateExternals :: Options -> AH.Prog -> String -> IO String
integrateExternals opts (AH.Prog m imps td fd od) fn = do
  exts <- lookupExternals opts (stripSuffix fn)
  let (pragmas, extimps, extdecls) = splitExternals exts
      prog' = AH.Prog m (imps ++ extimps) td fd od
  return $ unlines $ filter (not . null)
    [unlines (defaultPragmas : pragmas), showProg prog', unlines extdecls]

-- lookup an external file for a module and return either the content or an
-- empty String
lookupExternals :: Options -> String -> IO String
lookupExternals opts fn = do
  exists <- doesFileExist extName
  if exists
    then showDetail opts "External file found" >> readFile extName
    else showDetail opts "No External file found" >> return ""
    where extName = externalFile fn

-- Split an external file into a pragma String, a list of imports and the rest
-- TODO: This is a bloody hack
splitExternals :: String -> ([String], [String], [String])
splitExternals content = se (lines content) ([], [], []) where
  se [] res = res
  se (ln:lns) res
    | take 3 ln == "{-#" -- -} Comment to fix syntax highlighting in emacs
        = (ln : pragmas, imps, decls)
    | take 7 ln == "import " = (pragmas, drop 7 ln : imps, decls)
    | otherwise              = (pragmas, imps, ln : decls)
      where (pragmas, imps, decls) = se lns res

-- Dump an intermediate result to a file
dump :: DumpFormat -> Options -> String -> String -> IO ()
dump format opts file src = when (format `elem` opts -> optDump) $ do
  showDetail opts $ "Dumping " ++ file
  writeFileInDir (withDirectory (</> opts -> optOutputSubdir) file) src

rename :: Prog -> Prog
rename p@(Prog name imports _ _ _) =
  Prog (renameModule name) (map renameModule imports) td fd od where
  (Prog _ _ td fd od) = updQNamesInProg renameQName p

-- ---------------------------------------------------------------------------
-- IO state monad, like StateT IO
-- ---------------------------------------------------------------------------
data Mo s a = M (s -> IO (a, s))

unM :: Mo s a -> s -> IO (a, s)
unM (M x) = x

returnM :: a -> Mo s a
returnM x = M $ \s -> return (x, s)

bindM :: Mo s a -> (a -> Mo s b) -> Mo s b
bindM f g = M $ \s -> do
  (x, s') <- unM f s
  unM (g x) s'

bindM_ :: Mo st a -> Mo st b -> Mo st b
bindM_ f g = f `bindM` \_ -> g

getState :: Mo s s
getState = M $ \s -> return (s, s)

putState :: s -> Mo s ()
putState s = M $ \ _ -> return ((), s)

updState :: (s -> s) -> Mo s ()
updState f = getState `bindM` \s -> putState (f s)

liftIO :: IO a -> Mo s a
liftIO act = M $ \s -> do
  a <- act
  return (a, s)

mapM :: (a -> Mo s b) -> [a] -> Mo s [b]
mapM _ [] = returnM []
mapM f (m:ms) = f m       `bindM` \m' ->
                mapM f ms `bindM` \ms' ->
                returnM (m':ms')

-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

type TypeMap = FM QName QName

type State =
  { typeMap      :: TypeMap
  , ndResult     :: NDResult
  , hoResultFun  :: HOResult
  , hoResultCons :: HOResult
  , nextID       :: VarIndex    -- index for fresh variable
  , detMode      :: Bool        -- determinism mode
--   , report      :: [String]
  , compOptions  :: Options     -- compiler options
  }

defaultState :: State
defaultState =
  { typeMap      = listToFM (<) primTypes
  , ndResult     = initNDResult
  , hoResultFun  = initHOResult
  , hoResultCons = emptyFM (<)
  , nextID       = idVar
  , detMode      = False
--   , report      = []
  , compOptions  = defaultOptions
  }

type M a = Mo State a

-- type map

addTypeMap :: TypeMap -> M ()
addTypeMap newTypes =
 updState (\st -> { typeMap :=  st -> typeMap `plusFM` newTypes  | st })


getType :: QName -> M QName
getType qn = getState `bindM` \st ->
  returnM $ fromMaybe (error $ show qn ++ " not in type map" )
  $ (flip lookupFM) qn $ (st -> typeMap)

-- NDResult

addNDAnalysis :: NDResult -> M ()
addNDAnalysis newRes = updState $ \s -> { ndResult := newRes `plusFM` s -> ndResult | s }

getNDClass :: QName -> M NDClass
getNDClass qn = getState `bindM` \st ->
  returnM $ fromMaybe (error $ show qn ++ " not analysed" )
  $ (flip lookupFM) qn $ (st -> ndResult)
-- HOFunResult

addHOFunAnalysis :: HOResult -> M ()
addHOFunAnalysis newRes = updState$ \s -> { hoResultFun := newRes `plusFM` s -> hoResultFun | s }

getFunHOClass :: QName -> M HOClass
getFunHOClass qn = getState `bindM` \st ->
  returnM $ fromMaybe (error $ show qn ++ " not analysed" )
  $ (flip lookupFM) qn $ (st -> hoResultFun)

-- HOConsResult

addHOConsAnalysis :: HOResult -> M ()
addHOConsAnalysis newRes = updState$ \s -> { hoResultCons := (newRes `plusFM` s -> hoResultCons) | s }

getConsHOClass :: QName -> M HOClass
getConsHOClass qn = getState `bindM` \st ->
  returnM $ fromMaybe (error $ show qn ++ " not analysed" )
  $ (flip lookupFM) qn $ (st -> hoResultCons)

-- IDs

getNextID :: M Int
getNextID = getState `bindM` \st -> returnM (st -> nextID)

setNextID :: Int -> M ()
setNextID i = updState (\st -> { nextID := i | st })

takeNextID :: M Int
takeNextID =
  getState `bindM` \st ->
  let i = st -> nextID in
  putState ({ nextID := (i + 1) | st }) `bindM_`
  returnM i

takeNextIDs :: Int -> M [Int]
takeNextIDs n =
  getState `bindM` \st ->
  let i = st -> nextID in
  putState ({ nextID := (i + n) | st }) `bindM_`
  returnM [i .. i+n-1]

-- DetMode

isDetMode :: M Bool
isDetMode = getState `bindM` \st -> returnM (st -> detMode)

setDetMode :: Bool -> M ()
setDetMode dm = updState (\st -> { detMode := dm | st})

-- Perform an action in a given detMode and restore the original mode
-- afterwards
doInDetMode :: Bool -> M a -> M a
doInDetMode dm action =
  isDetMode `bindM` \ oldDm ->
  setDetMode dm `bindM_`
  action `bindM` \ retVal ->
  setDetMode oldDm `bindM_`
  returnM retVal

-- add a message to the transformation report
-- addToReport :: String -> M ()
-- addToReport msg = updState (\st -> {report := (msg : st -> report) | st})

-- Compiler options
getCompOptions :: M Options
getCompOptions = getState `bindM` \ st -> returnM (st -> compOptions)

getCompOption :: (Options -> a) -> M a
getCompOption select = getCompOptions `bindM` (returnM . select)

strictSupply :: M Bool
strictSupply = getCompOption $ \opts -> (opts -> optOptimization >= OptimStrictSupply)

-- ---------------------------------------------------------------------------
-- Program transformation
-- ---------------------------------------------------------------------------
transProg :: Prog -> M (Prog, AnalysisResult)
transProg p@(Prog m is ts fs _) =
  getState `bindM` \st ->
  let modNDRes     = analyseND     p (st -> ndResult)
      modHOResFun  = analyseHOFunc p (st -> hoResultFun)
      modHOResCons = analyseHOCons p
      modTypeMap   = getConsMap ts in
  addNDAnalysis     modNDRes      `bindM_`
  addHOFunAnalysis  modHOResFun  `bindM_`
  addHOConsAnalysis modHOResCons `bindM_`
  addTypeMap        modTypeMap   `bindM_`
  -- translation of the functions
  mapM transFunc fs `bindM` \fss ->
  returnM $ (Prog m is [] (concat fss) [], (modTypeMap, modNDRes, modHOResFun, modHOResCons))

-- Register the types of constructors to be able to retrieve the types for
-- constructors used in case patterns.
-- TODO: This becomes needless if the type could be computed from the
-- function's type expression, which in turn requires the case lifting to
-- provide correct types for lifted case expressions instead of TVar (-42).
getConsMap :: [TypeDecl] -> TypeMap
getConsMap ts =
  listToFM (<)
  $ concatMap (\ (Type qn _ _ cs) -> map (\c -> (consName c,qn)) cs)
  $ filter (not . isTypeSyn) ts

-- ---------------------------------------------------------------------------
-- Translation of Curry functions
-- ---------------------------------------------------------------------------

transFunc :: FuncDecl -> M [FuncDecl]
transFunc f@(Func qn _ _ _ _) =
  getCompOptions `bindM` \opts ->
  let opt = (opts -> optOptimization > OptimNone) in
  case opt of
    -- translate all functions as non-deterministic by default
    False -> transNDFunc f `bindM` \ fn -> returnM [fn]
    True  ->
      getNDClass qn `bindM` \ndCl ->
      getFunHOClass qn `bindM` \hoCl ->
      liftIO (showAnalysis opts (snd qn ++ " is " ++ show (ndCl, hoCl))) `bindM_`
      case ndCl of
        ND ->
          -- create non-deterministic function
          transNDFunc   f `bindM` \ fn ->
          returnM [fn]
        D -> case hoCl of
          FO ->
            -- create deterministic function
            transPureFunc f `bindM` \ fd ->
            returnM [fd]
          HO ->
            -- create deterministic as well as non-deterministic function
            transPureFunc f `bindM` \ fd ->
            transNDFunc   f `bindM` \ fn ->
            returnM [fd, fn]


-- translate into deterministic function
transPureFunc :: FuncDecl -> M FuncDecl
transPureFunc (Func qn a v t r) = doInDetMode True $
  renameFun qn `bindM` \qn' ->
  transRule (Func qn' a v t r) `bindM` \r' ->
  returnM (Func qn' (a + 1) v (check42 (transTypeExpr a) t) r')

-- translate into non-deterministic function
transNDFunc :: FuncDecl -> M FuncDecl
transNDFunc (Func qn a v t r) = doInDetMode False $
  renameFun qn `bindM` \qn' ->
  transRule (Func qn' a v t r) `bindM` \r' ->
  returnM (Func qn' (a + 2) v (check42 (transNDTypeExpr a) t) r')

-- renaming of functions respective to their order and the determinism mode
renameFun :: QName -> M QName
renameFun qn@(q, n) =
  isDetMode `bindM` \dm ->
  getNDClass qn `bindM` \ndCl ->
  getFunHOClass qn `bindM` \hoCl ->
  returnM (q, (funcPrefix dm ndCl hoCl) ++ n)

-- renaming of constructors respective to their order and the determinism mode
renameCons :: QName -> M QName
renameCons qn@(q, n) =
  isDetMode `bindM` \dm ->
  getConsHOClass qn `bindM` \hoCl ->
  returnM (q, (consPrefix dm hoCl) ++ n)

check42 :: (TypeExpr -> TypeExpr) -> TypeExpr -> TypeExpr
check42 f t = case t of
  (TVar (-42)) -> t
  _            -> f t

-- translate a type expressen by inserting an additional ConstStore type

transTypeExpr :: Int -> TypeExpr -> TypeExpr
transTypeExpr = transTypeExprWith (\t1 t2 -> FuncType t1 (FuncType storeType t2))
                                  (FuncType storeType)

-- translate a type expression by replacing (->) with Funcs and inserting
-- additional IDSupply and ConstStore types
transNDTypeExpr :: Int -> TypeExpr -> TypeExpr
transNDTypeExpr  = transTypeExprWith funcType 
                                     (FuncType supplyType . FuncType storeType)

transTypeExprWith :: (TypeExpr -> TypeExpr -> TypeExpr)
                    -> (TypeExpr -> TypeExpr)
                    -> Int -> TypeExpr -> TypeExpr
transTypeExprWith combFunc addArgs n t
    -- all arguments are applied
  | n == 0 = addArgs (transHOTypeExprWith combFunc t)
  | n >  0 = case t of
              (FuncType t1 t2) ->
                FuncType (transHOTypeExprWith combFunc t1) 
                         (transTypeExprWith combFunc addArgs (n-1) t2)
              _ -> error $ "transTypeExprWith: " ++ show (n, t)
  | n <  0 = error $ "transTypeExprWith: " ++ show (n, t)


-- transforms higher order type expressions using a function that combines
-- the two type-expressions of FuncTypes.
transHOTypeExprWith :: (TypeExpr -> TypeExpr -> TypeExpr) -> TypeExpr -> TypeExpr
transHOTypeExprWith _        t@(TVar _)       = t
transHOTypeExprWith combFunc (FuncType t1 t2) 
  = combFunc (transHOTypeExprWith combFunc t1)
             (transHOTypeExprWith combFunc t2)
transHOTypeExprWith combFunc (TCons qn ts)    = 
  TCons qn (map (transHOTypeExprWith combFunc) ts)

-- translate a single rule of a function adds a supply argument
-- to non-deterministic versions and a constStore argument to all functions
transRule :: FuncDecl -> M Rule
transRule (Func qn _ _ _ (Rule vs e)) =
  isDetMode `bindM` \ dm ->
  transBody qn vs e `bindM` \e' ->
  returnM $ Rule ((if dm then vs else vs ++ [suppVarIdx]) ++ [constStoreVarIdx]) e'
transRule (Func qn a _ _ (External _)) =
  isDetMode `bindM` \ dm ->
  let vs = [1 .. a] ++ (if dm then [] else [suppVarIdx]) ++ [constStoreVarIdx] in
  returnM $ Rule vs $ funcCall (externalFunc qn) (map Var vs)

transBody :: QName -> [Int] -> Expr -> M Expr
transBody qn vs exp = case exp of
  -- case expression with variable
  (Case ct e@(Var i) bs) ->
    -- translate branches
    mapM transBranch bs `bindM` \bs' ->
    -- create branches for non-deterministic constructors
    let bs'' = addUnifIntCharRule bs bs'
        pConsName = consNameFromPattern $ head bs in
    newBranches qn vs i pConsName `bindM` \ns ->
    -- TODO: superfluous?
    transExpr e `bindM` \(_, e') ->
    returnM $ Case ct e' (bs'' ++ ns)
  _ -> transCompleteExpr exp

addUnifIntCharRule :: [BranchExpr] -> [BranchExpr] -> [BranchExpr]
addUnifIntCharRule bs bs' =
  case bs of
   (Branch (LPattern (Intc  _)) _ :_) -> addRule True  bs bs' []
   (Branch (LPattern (Charc _)) _ :_) -> addRule False bs bs' []
   _                                 -> bs'
  where
    addRule isInt bs1 bs2 rules = case (bs1, bs2) of
      (Branch (LPattern lit) _ :nextBs, Branch p e:nextBs')
        -> Branch p e : addRule isInt nextBs nextBs' ((Lit lit,e):rules)

      -- TODO: magic number
      _ -> Branch (Pattern (constr isInt) [5000])
                  (funcCall (matchFun isInt)
                    [list2FCList $ map pair2FCPair $ reverse rules ,Var 5000,Var constStoreVarIdx ])
          : bs2
    matchFun True  = (basics,"matchInteger")
    matchFun False = (basics,"matchChar")
    constr   True  = renameQName (prelude,"CurryInt")
    constr   False = (curryPrelude,"CurryChar")

consNameFromPattern :: BranchExpr -> QName
consNameFromPattern (Branch (Pattern p _) _) = p
consNameFromPattern (Branch (LPattern lit) _) = case lit of
  Intc _   -> curryInt
  Floatc _ -> curryFloat
  Charc _  -> curryChar

-- translate case branch and return the name of the constructor
transBranch :: BranchExpr -> M BranchExpr
transBranch (Branch pat e) =
  transPattern pat `bindM` \pat' ->
  transCompleteExpr e `bindM` \e' ->
  returnM (Branch pat' e')

transPattern :: Pattern -> M Pattern
transPattern (Pattern qn vs) =
  renameCons qn `bindM` \qn' ->
  returnM (Pattern qn' vs)
transPattern l@(LPattern _) = returnM l

-- create new case branches for added non-deterministic constructors
-- qn'      : qualified name of the function currently processed
-- vs       : function arguments
-- i        : variable matched by case
-- pConsName: name of an arbitrary constructor of the type of the matched
--            variable
newBranches :: QName -> [Int] -> Int -> QName -> M [BranchExpr]
newBranches qn' vs i pConsName =
  isDetMode `bindM` \ dm ->
  -- lookup type name to create appropriate constructor names
  getType pConsName `bindM` \ typeName ->
  let Just pos = find (==i) vs
      suppVar = if dm then [] else [suppVarIdx]
      (vs1, _ : vs2) = break (==pos) vs
      call v = funcCall qn' $ map Var (vs1 ++ v : vs2 ++ suppVar ++ [constStoreVarIdx])
      -- pattern matching on guards will combine the new constraints with the given
      -- constraint store
      guardCall cVar valVar = strictCall (funcCall qn' $ map Var (vs1 ++ valVar : vs2 ++ suppVar))
      		     	      	       	 (combConstr cVar constStoreVarIdx)
      combConstr cVar constVar = funcCall combConstrName [Var cVar, Var constVar]
      -- EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL
      -- TODO: This is probably the dirtiest hack in the compiler:
      -- Because FlatCurry does not allow lambda abstractions, we construct
      -- a call to a function like "\f x1 x2 x-42 x3" which is replaced to
      -- the expression (\z -> f x1 x2 z x3) in the module
      -- FlatCurry2AbstractHaskell.
      -- EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL
      lCall = lambdaCall qn' $ map Var (vs1 ++ (-42) : vs2 ++ suppVar ++  [constStoreVarIdx]) in
  returnM $
    [ Branch (Pattern (mkChoiceName typeName) [1000, 1001, 1002])
             (liftOr [Var 1000, call 1001, call 1002])
    , Branch (Pattern (mkChoicesName typeName) [1000, 1001])
             (liftOrs [Var constStoreVarIdx, Var 1000, lCall, Var 1001])
    , Branch (Pattern (mkGuardName typeName) [1000, 1001])
             (liftGuard [Var 1000, guardCall 1000 1001])
    , Branch (Pattern ("", "_") [])
             liftFail
    ] -- TODO Magic numbers?

-- Complete translation of an expression where all newly introduced supply
-- variables are already bound by nested let expressions
transCompleteExpr :: Expr -> M Expr
transCompleteExpr e =
  strictSupply `bindM` \strict ->
  getNextID `bindM` \i -> -- save current variable id
  transExpr e `bindM` \(g, e') ->
  let e'' = case g of
              []  -> e'
              [v] ->  letIdVar strict [(v, Var suppVarIdx)] e' in
  setNextID i `bindM_` -- and reset it variable id
  returnM e''

-- transform an expression into a list of new supply variables to be bound
-- and the new expression
transExpr :: Expr -> M ([VarIndex], Expr)
-- variables
transExpr e@(Var _)        = returnM ([], e)

-- literals
transExpr (Lit (Intc   i)) = returnM ([], int   i)
transExpr (Lit (Floatc f)) = returnM ([], float f)
transExpr (Lit (Charc  c)) = returnM ([], char  c)

-- constructors
transExpr (Comb ConsCall qn es) =
  renameCons qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs `bindM` \(g, es') ->
  genIds g (Comb ConsCall qn' es')

-- calls to partially applied constructors are treated like calls to partially
-- applied deterministic first order functions.
transExpr (Comb (ConsPartCall i) qn es) =
  isDetMode `bindM` \dm ->
  renameCons qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs `bindM` \(g, es') ->
  genIds g (myWrap dm True D FO i (Comb (ConsPartCall i) qn' es'))

-- fully applied functions
transExpr (Comb FuncCall qn es) =
  getCompOption (\opts -> opts -> optOptimization > OptimNone) `bindM` \opt ->
  getNDClass qn `bindM` \ndCl ->
  getFunHOClass qn `bindM` \hoCl ->
  isDetMode `bindM` \dm ->
  renameFun qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs `bindM` \(g, es') ->
  if ndCl == D && opt && (hoCl == FO || (hoCl == HO && dm))
    then genIds g (Comb FuncCall qn' (es' ++ [Var constStoreVarIdx]))
    else takeNextID `bindM` \i ->
         genIds (i:g) (Comb FuncCall qn' (es' ++ [Var i, Var constStoreVarIdx]))

-- partially applied functions
transExpr (Comb (FuncPartCall i) qn es) =
  getCompOption (\opts -> opts -> optOptimization > OptimNone) `bindM` \opt ->
  getNDClass qn `bindM` \ndCl ->
  getFunHOClass qn `bindM` \hoCl ->
  isDetMode `bindM` \dm ->
  renameFun qn `bindM` \qn' ->
  mapM transExpr es `bindM` unzipArgs  `bindM` \(g, es') ->
  case ndCl of
    _ -> genIds g (myWrap dm opt ndCl hoCl i (Comb (FuncPartCall i) qn' es'))

    -- TODO: we do not care about higher order calls to nd functions right now
    -- _    -> takeNextID `bindM` \i ->
    --   genIds (i:g) (Comb FuncCall qn' (es' ++ [Var i]))

-- let expressions
transExpr (Let vses e) =
  let (vs,es) = unzip vses in
  mapM transExpr es `bindM` unzipArgs `bindM` \(g, es') ->
  transExpr e `bindM` \(ge, e') ->
  genIds (g ++ ge) (Let (zip vs es') e')

-- non-determinism
transExpr (Or e1 e2) = transExpr (qmark e1 e2)

-- free variable
transExpr (Free vs e) =
  transExpr e `bindM` \(g, e') ->
  takeNextIDs (length vs) `bindM` \is ->
  genIds (g++is) (Let (zipWith (\ v i -> (v,generate (Var i))) vs is) e')

-- case -- TODO give reasonable implementation
transExpr e@(Case _ _ _) = returnM ([], e)

genIds :: [VarIndex] -> Expr -> M ([VarIndex], Expr)
genIds [] expr = returnM ([], expr)
genIds ns@(_:_) expr =
  strictSupply `bindM` \strict ->
  -- get next free variable id
  getNextID `bindM` \i ->
  -- create splitting of supply variables
  let (vroot, v', vs) = mkSplits i ns 
      addSplit (v, v1, v2) e = letIdVar strict 
        [(v1, leftSupply [Var v]), (v2, rightSupply [Var v])] e
  in 
  setNextID v' `bindM_` returnM ([vroot], foldr addSplit expr vs)
  where
    
{-
  case vs of
    -- no splitting necessary
    [] -> returnM (ns, e)
    --
    _  -> setNextID (v'+1) `bindM_` returnM ([v'], foldr addSplit e vs)
 where addSplit (v, v1, v2) e' =
        lazyLet [(v1, leftSupply [Var v]), (v2, rightSupply [Var v])] e'
  -}

-- TODO magic numbers
idVar      = 2000

-- Variable index for supply variable
suppVarIdx = 3000

-- Variable index for constraint store
constStoreVarIdx = 3500

freshVars :: [Int] -> [Int]
freshVars used = filter (`elem` used) [0 .. ]

unzipArgs :: [([VarIndex], e)] -> M ([VarIndex], [e])
unzipArgs ises = returnM (concat is, es) where (is, es) = unzip ises

-- ---------------------------------------------------------------------------
-- Wrapping
-- ---------------------------------------------------------------------------

myWrap :: Bool -> Bool -> NDClass -> HOClass -> Int -> Expr -> Expr
myWrap True  _   _  _  a e = wrapCs a e
myWrap False opt nd ho a e = newWrap a iw (wrapCs a e)
  where iw = if opt && nd == D && ho == FO then wrapDX else wrapNX

-- adding Constraintstore arguments after every argument of a higher order funchtion
wrapCs :: Int -> Expr -> Expr
wrapCs n e | n == 1 = if isConstr then addCs [funId,e] else e
           | n >  1 = addCs [mkWraps (n - 1)
                              (if isConstr then (addCs [funId]) else funId)
                            ,e]
 where
  mkWraps m expr | m < 2     = expr
                 | otherwise = mkWraps (m - 1) (addCs [expr])
  isConstr = case e of
    (Comb (ConsPartCall _) _ _) -> True
    _                           -> False

newWrap :: Int -> ([Expr] -> Expr) -> Expr -> Expr
newWrap n innermostWrapper e
  | n == 0 = e
  | n == 1 = innermostWrapper [funId, e]
  | n == 2 = wrapDX [innermostWrapper [funId], e]
  | n == 3 = wrapDX [wrapDX [innermostWrapper [funId]], e]
  | n == 4 = wrapDX [wrapDX [wrapDX [innermostWrapper [funId]]], e]
  | n >  4 = wrapDX [wraps (n-1) (innermostWrapper [funId]), e]
  where wraps m expr = if m <= 1 then expr else wrapDX [wraps (m - 1) expr]

wrapDX exprs = fun 2 (basics,"wrapDX") exprs
wrapNX exprs = fun 2 (basics,"wrapNX") exprs
addCs  exprs = fun 2 (basics,"addCs")  exprs
funId = fun 1 (prelude,"id") []


-- Strict or lazy computation of supplies
letIdVar True  = strictLet
letIdVar False = lazyLet

-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

prelude :: String
prelude = "Prelude"

basics :: String
basics = "Basics"

curryPrelude :: String
curryPrelude = renameModule "Prelude"

curryInt :: QName
curryInt = renameQName (prelude, "Int")

curryFloat :: QName
curryFloat = renameQName (prelude, "Float")

curryChar :: QName
curryChar = renameQName (prelude, "Char")

combConstrName :: QName
combConstrName = (basics,"combConstr")

-- type expressions

tOrRef :: TypeExpr
tOrRef = TCons (basics, "ID") []

tConstraint :: TypeExpr
tConstraint = TCons (basics, "Constraint") []

supplyType :: TypeExpr
supplyType  = TCons (basics, "IDSupply") []

storeType :: TypeExpr
storeType = TCons (basics, "ConstStore") []

funcType :: TypeExpr -> TypeExpr -> TypeExpr
funcType t1 t2 = TCons (basics, "Func") [t1, t2]

-- expressions

list2FCList :: [Expr] -> Expr
list2FCList [] = consCall (prelude, "[]") []
list2FCList (e:es) = consCall (prelude, ":") [e,list2FCList es]

pair2FCPair :: (Expr,Expr) -> Expr
pair2FCPair (e1,e2) = consCall (prelude, "(,)") [e1,e2]

lazyLet :: [(Int, Expr)] -> Expr -> Expr
lazyLet decls e = Let decls e

strictLet :: [(Int, Expr)] -> Expr -> Expr
strictLet decls e = Let decls $ foldr seqCall e $ map (Var . fst) decls

seqCall :: Expr -> Expr -> Expr
seqCall e1 e2 = funcCall (prelude, "seq") [e1, e2]

strictCall :: Expr -> Expr -> Expr
strictCall f e = funcCall (prelude, "$!") [f,e]

funcCall n xs = Comb FuncCall n xs
lambdaCall (q,n) xs = Comb FuncCall (q, '\\' : n) xs
consCall n xs = Comb ConsCall n xs
constant qn = consCall qn []

fun :: Int -> QName -> [Expr] -> Expr
fun i n xs | length xs == i = funcCall n xs
           | otherwise      = Comb (FuncPartCall (length xs - i)) n xs

int :: Int -> Expr
int i = funcCall curryInt [constant (prelude, showInt i ++ "#")]

char :: Char -> Expr
char c = funcCall curryChar charExpr
  where
  charExpr
    | ord c < 127 = [constant (prelude, showLiteral (AH.Charc c) ++ "#")]
      -- due to problems with non-ASCII characters in ghc
    | otherwise   = [funcCall (basics, "nonAsciiChr")
                              [constant (prelude, show (ord c) ++ "#")]]

float :: Float -> Expr
float f = funcCall curryFloat [constant (prelude, showFloat f ++ "#")]

liftOr      = funcCall (basics, "narrow")
liftOrs     = funcCall (basics, "narrows")
liftGuard   = funcCall (basics, "guardCons")
liftFail    = funcCall (basics, "failCons") []
qmark e1 e2 = funcCall (renameQName (prelude, "?")) [e1, e2]

splitSupply = funcCall (basics, "splitSupply")
initSupply  = funcCall (basics, "initIDSupply") []
leftSupply  = funcCall (basics, "leftSupply")
rightSupply = funcCall (basics, "rightSupply")
generate i  = funcCall (basics, "generate") [i]

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------



defaultPragmas :: String
defaultPragmas = "{-# LANGUAGE MagicHash #-}"

defaultModules :: [String]
defaultModules = [basics]

-- list of known primitive types
primTypes :: [(QName, QName)]
primTypes = map (\ (x, y) -> ( renameQName (prelude, x)
                             , renameQName (prelude, y))) $
  [ ("True", "Bool"), ("False", "Bool")
  , ("Int", "Int")  , ("Float", "Float"), ("Char", "Char")]

-- Return Nothing if type is no tuple and Just arity otherwise
tupleArity :: String -> Maybe Int
tupleArity s
  | arity > 1 && s == '(' : replicate (arity -1) ',' ++ ")" = Just arity
  | otherwise                                               = Nothing
  where arity = length s - 1

maxTupleArity :: Int
maxTupleArity = 15

tupleType :: Int -> String
tupleType arity = '(' : replicate (arity - 1) ',' ++ ")"
