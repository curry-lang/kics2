------------------------------------------------------------------------------
--- Library to annotate the expressions of a FlatCurry program
--- with type information.
---
--- It can be used by any other Curry program which processes
--- or transforms FlatCurry programs. The main operation to use is
---
---     inferProg :: Prog -> IO (Either String (AProg TypeExpr))
---
--- which annotates a FlatCurry program with type information.
---
--- The type inference works in several steps:
---
---  1. For each known function and constructor, either imported or defined
---     in the module itself, the respective type is inserted into a type
---     environment (type assumption).
---
---  2. Every subexpression is annotated with a fresh type variable, whereas
---     constructor and function names are annotated with a fresh variant of
---     the type in the type assumption.
---
---  3. Based on FlatCurry's type inference rules, type equations are generated
---     for a function's rule.
---
---  4. The resulting equations are solved using unification and the
---     resulting substitution is applied to the function rule.
---
---  5. The inferred types are then normalized such that for every function
---     rule the type variables start with 0.
---
--- In addition, the function `inferNewFunctions` allows to infer the types
--- of a list of functions whose type is not known before. Consequently,
--- this disallows polymorphic recursive functions. Those functions are
--- separated into strongly connected components before their types are inferred
--- to allow mutually recursive function definitions.
---
--- In case of any error, the type inference quits with an error message.
---
--- @author  Jonas Oberschweiber, Bjoern Peemoeller, Michael Hanus
--- @version August 2016
--- @category meta
------------------------------------------------------------------------------

module FlatCurry.Annotated.TypeInference
  ( TypeEnv, getTypeEnv, getTypeEnvFromProgEnv
  , inferProg, inferProgFromProgEnv, inferProgEnv
  , inferFunction, inferFunctionEnv
  , inferNewFunctions, inferNewFunctionsEnv
  , inferExpr, inferExprEnv
  ) where

import           FiniteMap
import           List                               (find)
import qualified Pretty as P

import           ErrorState
import           FlatCurry.Types
import           FlatCurry.Files
import           FlatCurry.Goodies                  (branchExpr, funcName)
import           FlatCurry.Annotated.Types
import qualified FlatCurry.Annotated.Goodies as AFC (annExpr, funcName)
import           FlatCurry.Annotated.Pretty         (ppQName, ppExp, ppTypeExp
                                                    , ppVarIndex)
import           FlatCurry.Annotated.TypeSubst
import           SCC
import           Rewriting.Term
import           Rewriting.Unification
import           Unsafe

-- ---------------------------------------------------------------------------
-- Public Interface
-- ---------------------------------------------------------------------------

--- Infers the type of a whole program.
---
--- @param p - the Prog to infer
--- @return the inferred program or an error
inferProg :: Prog -> IO (Either String (AProg TypeExpr))
inferProg p = getTypeEnv p >>= \te -> return (inferProgEnv te p)

--- Infers the type of a whole program w.r.t. a list of imported modules.
---
--- @param p - the Prog to infer
--- @return the inferred program or an error
inferProgFromProgEnv :: [(String, Prog)] -> Prog
                     -> Either String (AProg TypeExpr)
inferProgFromProgEnv env p = case getTypeEnvFromProgEnv env p of
  Left err    -> Left err
  Right tyEnv -> inferProgEnv tyEnv p

--- Infers the types of a single function specified by its qualified name.
---
--- @param p - the Prog containing the function
--- @param q - the qualified name of the function
--- @return the inferred function or an error
inferFunction :: Prog -> QName -> IO (Either String (AFuncDecl TypeExpr))
inferFunction p f = getTypeEnv p >>= \te -> return (inferFunctionEnv te p f)

--- Infers the types of a group of (possibly mutually recursive) functions.
--- Note that the functions are only monomorphically instantiated, i.e.,
--- polymorphic recursion is not supported.
--- The given type may be too general, for instance a type variable only,
--- and will be specialised to the inferred type.
inferNewFunctions :: Prog -> [FuncDecl]
                  -> IO (Either String [AFuncDecl TypeExpr])
inferNewFunctions p@(Prog mid _ _ _ _) fs
  = getTypeEnv p >>= \te -> return (inferNewFunctionsEnv te mid fs)

--- Infer the type of a single expression.
---
--- @param p - the Prog containing the function
--- @param e - the expression
--- @return the inferred expression or an error
inferExpr :: Prog -> Expr -> IO (Either String (AExpr TypeExpr))
inferExpr p e = getTypeEnv p >>= \te -> return (inferExprEnv te e)

-- ---------------------------------------------------------------------------

--- Infers the type of a whole program.
--- Uses the given type environment instead of generating a new one.
---
--- @param te - the type environment
--- @param p  - the Prog to infer
--- @return the inferred program or an error
inferProgEnv :: TypeEnv -> Prog -> Either String (AProg TypeExpr)
inferProgEnv te p = evalES (annProg p >+= inferAProg) (initTIM te)

--- Infers the types of a single function specified by its qualified name.
--- Uses the given type environment instead of generating a new one.
---
--- @param te  - the type environment
--- @param p   - the Prog containing the function
--- @param fun - the qualified name of the function
--- @return the inferred function or an error
inferFunctionEnv :: TypeEnv -> Prog -> QName
                 -> Either String (AFuncDecl TypeExpr)
inferFunctionEnv te (Prog _ _ _ fs _) f = case find ((== f) . funcName) fs of
  Nothing -> Left $ P.pretty 80 $ P.text "No such function:" P.<+> ppQName f
  Just fd -> evalES (annFunc fd >+= inferFunc) (initTIM te)

--- Infers the types of a group of (possibly mutually recursive) functions.
--- Note that the functions are only monomorphically instantiated, i.e.,
--- polymorphic recursion is not supported.
--- The given type may be too general, for instance a type variable only,
--- and will be specialised to the inferred type.
inferNewFunctionsEnv :: TypeEnv -> String -> [FuncDecl]
                     -> Either String [AFuncDecl TypeExpr]
inferNewFunctionsEnv te mid fs = evalES (infer (depGraph mid fs)) (initTIM te)
  where
  infer fss     = concatMapES inferGroup fss >+= \fs' ->
                  mapES (flip extract fs') fs
  inferGroup g  = annFuncGroup g >+= inferFuncGroup                 >+= \afs ->
                  extendTypeEnv [(f, ty) | AFunc f _ _ ty _ <- afs] >+
                  returnES afs
  extract f afs = case find ((== funcName f) . AFC.funcName) afs of
    Just af -> returnES af
    Nothing -> failES "Internal error: extract"

--- Infers the types of a single expression.
--- Uses the given type environment instead of generating a new one.
---
--- @param te - the type environment
--- @param e  - the expression
--- @return the inferred expression or an error
inferExprEnv :: TypeEnv -> Expr -> Either String (AExpr TypeExpr)
inferExprEnv te e = evalES (annExpr e >+= inferAExpr) (initTIM te)

-- ---------------------------------------------------------------------------
-- Computation of dependency graph and strongly connected components
-- ---------------------------------------------------------------------------

--- Compute the dependency graph of functions and separate it into strongly
--- connected components.
depGraph :: String -> [FuncDecl] -> [[FuncDecl]]
depGraph mid = scc def use
  where
  def (Func f _ _ _ _) = [f]
  use (Func _ _ _ _ r) = case r of
    Rule _ e -> called e
    _        -> []

  called (Var _) = []
  called (Lit _) = []
  called (Comb ct f@(m, _) es)
    | m == mid  = (case ct of
        FuncCall       -> [f]
        FuncPartCall _ -> [f]
        _              -> []) ++ concatMap called es
    | otherwise = concatMap called es
  called (Let    bs e) = concatMap called (map snd bs ++ [e])
  called (Free    _ e) = called e
  called (Or      a b) = concatMap called [a, b]
  called (Case _ e bs) = concatMap called (e : map branchExpr bs)
  called (Typed   e _) = called e

-- ---------------------------------------------------------------------------
-- 1. Type environment
-- ---------------------------------------------------------------------------

--- A type environment.
type TypeEnv = FM QName TypeExpr

--- Looks up a type with a qualified name in a type environment.
---
--- @param env - the type environment
--- @param q   - the qualified name to look for
--- @return maybe the type
lookupType :: TypeEnv -> QName -> Maybe TypeExpr
lookupType = lookupFM

--- Extract the type environment from the given Prog.
---
--- @param p - the Prog
--- @return a type environment
getTypeEnv :: Prog -> IO TypeEnv
getTypeEnv p = do
  is <- extractImported p
  return (extractKnownTypes $ p : is)

--- Reads the interfaces of all modules imported into the given Prog.
---
--- @param p - the Prog whose imports should be read
--- @return the list of interface Progs
extractImported :: Prog -> IO [Prog]
extractImported (Prog _ is _ _ _) = mapIO readFlatCurryInt is

--- Extract the type environment from the given Prog by lookup in a
--- module name -> Prog environment.
---
--- @param env - An environment mapping module names to Progs
--- @param p - the Prog
--- @return a type environment
getTypeEnvFromProgEnv :: [(String, Prog)] -> Prog -> Either String TypeEnv
getTypeEnvFromProgEnv env prog@(Prog _ imps _ _ _) = case extract imps of
  Left  err  -> Left  err
  Right mods -> Right (extractKnownTypes $ prog : mods)
 where
  extract []     = Right []
  extract (i:is) = case lookup i env of
    Nothing -> Left $ "getTypeEnvFromProgEnv: Could not find module " ++ i
    Just p  -> case extract is of
      Left err -> Left err
      Right ps -> Right (p : ps)

--- Extracts the type information of all function and datatype
--- declarations from the given list of Progs.
---
--- @param ps - the list of Progs
--- @return a type environment
extractKnownTypes :: [Prog] -> TypeEnv
extractKnownTypes ps = listToFM (<) $ concatMap extractProg ps
 where
  extractProg :: Prog -> [(QName, TypeExpr)]
  extractProg (Prog _ _ td fd _)
    = concatMap extractTypeDecl td ++ map extractFuncDecl fd

  extractFuncDecl :: FuncDecl -> (QName, TypeExpr)
  extractFuncDecl (Func n _ _ ty _) = (n, ty)

  extractTypeDecl :: TypeDecl -> [(QName, TypeExpr)]
  extractTypeDecl (TypeSyn  n _ _ ty) = [(n, ty)]
  extractTypeDecl (Type    n _ vs cs) = map (extractConsDecl ty) cs
    where ty = TCons n (map TVar vs)

  extractConsDecl :: TypeExpr -> ConsDecl -> (QName, TypeExpr)
  extractConsDecl ty (Cons n _ _ tys) = (n, foldr FuncType ty tys)

  typeArity :: TypeExpr -> Int
  typeArity ty = case ty of
    FuncType _ b -> 1 + typeArity b
    _            -> 0

-- ---------------------------------------------------------------------------
-- Type Inference Monad
-- ---------------------------------------------------------------------------

--- The monad contains an `Int` value for fresh type variable generation
--- and a mapping from variable indices to their associated type
--- variables. It returns a `String` if an error occured.
type TIM a = ES String (TypeEnv, Int, TypeEnv, FM Int TypeExpr) a

--- Initial TIM state.
initTIM :: TypeEnv -> (TypeEnv, Int, TypeEnv, FM Int TypeExpr)
initTIM te = (te, 0, emptyFM (<), emptyFM (<))

extendTypeEnv :: [(QName, TypeExpr)] -> TIM ()
extendTypeEnv ftys = gets >+= \ (te, v, fe, ve) ->
                     puts (addListToFM te ftys, v, fe, ve)

--- Retrieve the next fresh type variable.
nextTVar :: TIM TypeExpr
nextTVar = gets >+= \ (te, n, fun2Ty, var2Ty) ->
           puts (te, n + 1, fun2Ty, var2Ty) >+ returnES (TVar n)

--- Intialize the mapping from variables to type variables.
initVarTypes :: TIM ()
initVarTypes = modify $ \ (te, n, fe, _) -> (te, n, fe, emptyFM (<))

--- Intialize the type signature environment, i.e., delete all associations.
initSigEnv :: TIM ()
initSigEnv = modify $ \ (te, n, _, ve) -> (te, n, emptyFM (<), ve)

--- Insert a new variable/type variable association.
insertVarType :: Int -> TypeExpr -> TIM ()
insertVarType v ty = modify $ \ (te, n, fe, var2Ty) ->
                                 (te, n, fe, addToFM var2Ty v ty)

--- Insert a new function/type signature association.
insertFunType :: QName -> TypeExpr -> TIM ()
insertFunType f sig = freshVariant sig >+= \ty ->
                      modify $ \ (te, n, fe, ve) -> (te, n, addToFM fe f ty, ve)

--- Look up the type variable associated to a variable.
lookupVarType :: Int -> TIM (Maybe TypeExpr)
lookupVarType v = gets >+= \ (_, _, _, var2Ty) -> returnES (lookupFM var2Ty v)

--- Looks up a type in the type assumption or the type signature environment.
--- Types found in the type assumption are instantiated to a fresh variant.
---
--- @param q - the qualified name of the type to look up
--- @return its type
getTypeVariant :: QName -> TIM (QName, TypeExpr)
getTypeVariant f = gets >+= \ (env, _, fe, _) -> case lookupType env f of
  Nothing -> case lookupFM fe f of
    Nothing -> failES $ "Internal error: getTypeVariant " ++ show f
    Just ty -> returnES (f, ty)
  Just t  -> freshVariant t >+= \ty -> returnES (f, ty)

--- Compute a fresh variant of a given type expression.
---
--- @param ty - the type expression
--- @return The renamed type expression
freshVariant :: TypeExpr -> TIM TypeExpr
freshVariant ty = snd <$> rename [] ty
 where
  rename ren (TVar       i) = case lookup i ren of
    Just j  -> returnES (ren, j)
    Nothing -> nextTVar >+= \j -> returnES ((i, j) : ren, j)
  rename ren (FuncType a b) = rename ren  a >+= \ (ren1, a') ->
                              rename ren1 b >+= \ (ren2, b') ->
                              returnES (ren2, FuncType a' b')
  rename ren (TCons  t tys) = mapAccumES rename ren tys >+= \(ren', tys') ->
                              returnES (ren', TCons t tys')

-- -----------------------------------------------------------------------------
-- 2. Annotation, traversing the AST and inserting fresh type variables
-- -----------------------------------------------------------------------------

--- Converts the Prog to an AProg, inserting TVars into all expressions.
---
--- @param prog - the prog to convert
--- @return an AProg and the next TVar number in an TIM
annProg :: Prog -> TIM (AProg TypeExpr)
annProg (Prog mid is td fd od) =
  (\afd -> AProg mid is td afd od) <$> mapES annFunc fd

annFuncGroup :: [FuncDecl] -> TIM [AFuncDecl TypeExpr]
annFuncGroup fs = initSigEnv >+ mapES (uncurry insertFunType) ftys >+
                  mapES annFunc fs
  where ftys = [ (f, ty) | Func f _ _ ty _ <- fs]

--- Converts the FuncDecl to an AFuncDecl, inserting TVars into all
--- expressions.
annFunc ::FuncDecl -> TIM (AFuncDecl TypeExpr)
annFunc (Func qn a v _ r)
  = initVarTypes >+ AFunc qn a v <$> (snd <$> getTypeVariant qn) <*> annRule r

--- Converts the Rule to an ARule, inserting TVars into all expressions.
annRule :: Rule -> TIM (ARule TypeExpr)
annRule (Rule  vs e) = ARule <$> nextTVar <*> mapES annVar vs <*> annExpr e
annRule (External s) = flip AExternal s <$> nextTVar

--- Converts the Expr to an AExpr, inserting TVars into all sub-expressions.
annExpr :: Expr -> TIM (AExpr TypeExpr)
annExpr (Var       i) = lookupVarType i >+=
                        maybe (failES err) (\ty -> returnES (AVar ty i))
  where err = P.pretty 80 $ P.text "Variable" P.<+> ppVarIndex i
                      P.<+> P.text "was not initialized with a type"
annExpr (Lit       l) = nextTVar >+= \ty -> returnES (ALit ty l)
annExpr (Comb t q es) = flip AComb t <$> nextTVar <*> getTypeVariant q
                                                  <*> mapES annExpr es
annExpr (Case t e bs) = flip ACase t <$> nextTVar <*> annExpr e
                                                  <*> mapES annBranch bs
annExpr (Or      a b) = AOr  <$> nextTVar <*> annExpr     a  <*> annExpr b
annExpr (Let    ds e) = ALet <$> nextTVar <*> annBindings ds <*> annExpr e
 where annBindings bs = let (vs, es) = unzip bs in
                        mapES annBound vs >+= \vs' ->
                        mapES annExpr  es >+= \es' ->
                        returnES (zip vs' es')
       annBound v     = checkShadowing v >+ annVar v
annExpr (Free   vs e) = AFree <$> nextTVar <*> mapES annFree vs <*> annExpr e
  where annFree v     = checkShadowing v >+ annVar v
annExpr (Typed  e ty) = ATyped <$> nextTVar <*> annExpr e <*> freshVariant ty

--- Annotate a variable with a fresh type variable.
annVar :: VarIndex -> TIM (VarIndex, TypeExpr)
annVar v = nextTVar >+= \ty -> insertVarType v ty >+ returnES (v, ty)

--- Checks whether a locally introduced variable is already defined in the
--- surrounding scope, which indicates variable shadowing that is not allowed
--- in FlatCurry files. This is our basic assumption in this type inferencer,
--- and must therefore be met. Otherwise, the type inference must be extended.
checkShadowing :: VarIndex -> TIM ()
checkShadowing v = lookupVarType v >+= maybe (returnES ()) (\_ -> failES err)
  where err = P.pretty 80 $ P.text "shadowing with variable" P.<+> ppVarIndex v

--- Converts the BranchExpr to an ABranchExpr, inserting TVars
--- into all expressions
---
--- @param n - the first TVar number to use
--- @return the ABranchExpr and the new next TVar number in an TIM
annBranch :: BranchExpr -> TIM (ABranchExpr TypeExpr)
annBranch (Branch p e) = ABranch <$> annPattern p <*> annExpr e

--- Converts the Pattern into an APattern, inserting a TVar
--- into the pattern
---
--- @param n - the TVar number to use
--- @return the APattern and the new next TVar number in an TIM
annPattern :: Pattern -> TIM (APattern TypeExpr)
annPattern (Pattern c vs) = APattern <$> nextTVar <*> getTypeVariant c
                                                  <*> mapES annPVar vs
  where annPVar v = checkShadowing v >+ annVar v
annPattern (LPattern   l) = flip ALPattern l <$> nextTVar

-- ---------------------------------------------------------------------------
-- 3. Type inference
-- ---------------------------------------------------------------------------

--- Type equations
type TypeEqs = [(TypeExpr, TypeExpr)]

--- Smart constructor for type equation
(=.=) :: TypeExpr -> TypeExpr -> (TypeExpr, TypeExpr)
ty1 =.= ty2 = (ty1, ty2)

ppTypeEqs :: TypeEqs -> P.Doc
ppTypeEqs = P.vsep . map ppEquation
  where ppEquation (l, r) = ppTypeExp l P.<+> P.equals P.<+> ppTypeExp r

--- Append two lists yielded by monadic computations.
(++=) :: TIM [a] -> TIM [a] -> TIM [a]
mxs ++= mys = (++) <$> mxs <*> mys

--- Infers all types in the given program.
---
--- @param p - the program to infer
--- @param n - the next fresh TVar number
--- @return the inferred program or an error
inferAProg :: AProg TypeExpr -> TIM (AProg TypeExpr)
inferAProg (AProg mid is td fd od)
  = (\fd' -> AProg mid is td fd' od) <$> mapES inferFunc fd

--- Infers all types in the given function declaration group.
inferFuncGroup :: [AFuncDecl TypeExpr] -> TIM [AFuncDecl TypeExpr]
inferFuncGroup fs =
--   returnES (unsafePerformIO (mapIO_ print fs)) >+= \() ->
  concatMapES (uncurry eqsRule) [(ty, r) | AFunc _ _ _ ty r <- fs] >+= \eqs ->
--   returnES (unsafePerformIO (putStrLn (ppTypeEqs eqs))) >+= \() ->
  solve (P.text "functions" P.<+> doc) eqs >+= \ sigma ->
--   returnES (unsafePerformIO (putStrLn (showAFCSubst sigma))) >+= \() ->
  mapES (normalize normFunc . substFunc sigma) fs >+= \afs ->
--   returnES (unsafePerformIO (mapIO_ print afs)) >+= \() ->
  returnES afs
  where doc = P.hsep $ P.punctuate P.comma (map (ppQName . AFC.funcName) fs)

--- Infers all types in the given function.
---
--- @param n - the next fresh TVar number
--- @param f - the function
--- @return the inferred function or an error
inferFunc :: AFuncDecl TypeExpr -> TIM (AFuncDecl TypeExpr)
inferFunc func@(AFunc _ _ _ ty r) =
  eqsRule ty r >+= \ eqs    ->
  solve (P.text "function" P.<+> ppQName (AFC.funcName func)) eqs >+= \ sigma ->
  normalize normFunc (substFunc sigma func)

--- Infer the type of an expression.
inferAExpr :: AExpr TypeExpr -> TIM (AExpr TypeExpr)
inferAExpr e = eqsExpr e >+= \eqs   ->
               solve (P.text "expression" P.<+> ppExp e) eqs >+= \sigma ->
               normalize normExpr (substExpr sigma e)

--- Infer the type for a rule.
eqsRule :: TypeExpr -> ARule TypeExpr -> TIM TypeEqs
eqsRule ty (ARule     ty2 vs e)
  = returnES [ty =.= ty2, ty2 =.= foldr1 FuncType (map snd vs ++ [exprType e])]
    ++= eqsExpr e
eqsRule ty (AExternal ty2    _) = returnES [ty =.= ty2]

--- Recursively generate equations for unification from an expression.
---
--- @param e - the expression
--- @return a list of type equations generated from `e`.
eqsExpr :: AExpr TypeExpr -> TIM TypeEqs
-- No equations to generate.
eqsExpr (AVar _  _)       = returnES []
-- The type of the expression is equal to the type of the literal.
eqsExpr (ALit ty l)       = returnES [ty =.= literalType l]
-- Match the types of the argument expressions and the result type
-- to the type of the function or constructor.
eqsExpr (AComb ty _ (_, fty) es)
  = returnES [fty =.= foldr1 FuncType (map exprType es ++ [ty])]
    ++= concatMapES eqsExpr es
-- Generate equations for the subject and the branches.
eqsExpr (ACase ty _ e bs) = eqsExpr e ++= concatMapES (eqsBranch ty e) bs
-- The type of the expression must be equal to the types
-- of both argument expressions.
eqsExpr (AOr ty a b) = returnES [exprType a =.= ty, exprType b =.= ty]
                       ++= eqsExpr a ++= eqsExpr b
-- The type of the expression must be equal to the type of the inner expression.
-- The type of each bound variable must be equal to the type of the
-- corresponding expression.
eqsExpr (ALet ty bs e)
  =     returnES [ty =.= exprType e]
    ++= returnES (map (\ ((_, vty), b) -> vty =.= exprType b) bs)
    ++= concatMapES eqsExpr (e : map snd bs)
-- The type of the expression itself must be equal to the type
-- of the inner expression.
eqsExpr (AFree ty   _ e) = returnES [ty =.= exprType e] ++= eqsExpr e
-- The type of the expression must be equal to the type of the argument
-- expression and to the specified type.
eqsExpr (ATyped ty e tz) = returnES [ty =.= exprType e, ty =.= tz] ++= eqsExpr e

--- Generate equations for a branch.
---
---  - equating the type of the branch's expression to the type of the
---    overall case expression
---  - generating equations for the branch's pattern
---  - generating equations for the branch's expression
---
--- @param ty   - the parent case expression's type
--- @param subj - the case's subject expression
--- @param b    - the branch
eqsBranch :: TypeExpr -> AExpr TypeExpr -> ABranchExpr TypeExpr -> TIM TypeEqs
eqsBranch ty s (ABranch p be) = returnES [ty =.= exprType be]
  ++= eqsPattern (exprType s) p ++= eqsExpr be

--- Generate equations for a pattern.
---
---  - Equate the type of the case's subject to the type of the pattern.
---  - For constructor patterns: Equate the type of the argument patterns and
---    the type of the subject to the type of the constructor.
---  - For literal patterns: Equate the type of the pattern to the type
---    of the literal.
eqsPattern :: TypeExpr -> APattern TypeExpr -> TIM TypeEqs
eqsPattern ty (APattern  pty (_, cty) vs)
  = returnES [ty =.= pty, cty =.= foldr1 FuncType (map snd vs ++ [pty])]
eqsPattern ty (ALPattern pty           l)
  = returnES [ty =.= pty, pty =.= literalType l]

--- Extract the type of a Literal.
literalType :: Literal -> TypeExpr
literalType (Intc   _) = TCons ("Prelude", "Int"  ) []
literalType (Floatc _) = TCons ("Prelude", "Float") []
literalType (Charc  _) = TCons ("Prelude", "Char" ) []

--- Extract the TypeExpr from an annotated Expr.
exprType :: AExpr TypeExpr -> TypeExpr
exprType = AFC.annExpr

-- ---------------------------------------------------------------------------
-- 4. Functions for interfacing with the Unification module
-- ---------------------------------------------------------------------------

--- Solve a list of type equations using unification.
solve :: P.Doc -> TypeEqs -> TIM AFCSubst
solve what eqs = case unify (fromTypeEqs eqs) of
  Left  err -> failES $ P.pretty 80 $ ppUnificationError err
                 P.<+> P.text "during type inference for:" P.<$$> P.nest 2 what
  Right sub -> returnES (mapFM (\_ -> toTypeExpr) sub)

--- Converts a list of type expression equations into term equations.
fromTypeEqs :: TypeEqs -> TermEqs String
fromTypeEqs = map (\(a,b) -> (fromTypeExpr a, fromTypeExpr b))

--- Converts a list of term equations into type expression equations.
toTypeEqs :: TermEqs String -> TypeEqs
toTypeEqs = map (\(a,b) -> (toTypeExpr a =.= toTypeExpr b))

--- Converts the given type expression into a term for unification.
fromTypeExpr :: TypeExpr -> Term String
fromTypeExpr (TVar       n) = TermVar n
fromTypeExpr (TCons   t vs) = TermCons (fromQName t) (map fromTypeExpr vs)
fromTypeExpr (FuncType a b) = TermCons "->" [fromTypeExpr a, fromTypeExpr b]

--- Converts the given unification term into a type expression
toTypeExpr :: Term String -> TypeExpr
toTypeExpr (TermVar     n) = TVar n
toTypeExpr (TermCons t vs)
    | t == "->" = FuncType (toTypeExpr (vs !! 0)) (toTypeExpr (vs !! 1))
    | otherwise = TCons (toQName t) (map toTypeExpr vs)

--- Converts a qualified name to a string.
fromQName :: QName -> String
fromQName (mod, typ) = mod ++ ";" ++ typ

--- Converts a string to a qualified name.
toQName :: String -> QName
toQName str = (fst split, snd split)
  where split = splitFirst str ';'

--- Splits a list at the first occurence of a given value.
---
--- @param xs - the list to split
--- @param x - the value to split at
--- @return a tuple of the lists before and after the split
splitFirst :: [a] -> a -> ([a], [a])
splitFirst []     _ = ([], [])
splitFirst (x:xs) c
  | x == c    = ([], xs)
  | otherwise = (x : fst rest, snd rest)
    where rest = splitFirst xs c

--- Formats a unification error with the given message.
ppUnificationError :: UnificationError String -> P.Doc
ppUnificationError (Clash      a b)
  = P.text "Clash:" P.<+> ppTypeExp (toTypeExpr a) P.<+> P.equals
                    P.<+> ppTypeExp (toTypeExpr b)
ppUnificationError (OccurCheck v t)
  = P.text "OccurCheck: Type variable" P.<+> ppTypeExp (toTypeExpr (TermVar v))
      P.<+> P.text "occurs in type" P.<+> ppTypeExp (toTypeExpr t)

-- ---------------------------------------------------------------------------
-- 5. Functions for normalization of type variables.
--    Renumbers type variables in a function starting from 0.
-- ---------------------------------------------------------------------------

-- We need to keep the next fresh variable number to assign and a mapping from
-- existing variable numbers to newly assigned ones.
-- Note that we do not need the error functionality but reuse the ES monad
-- to avoid writing our own state monad.
type NormState   = (Int, FM Int Int)
type Normalize a = a -> ES () NormState a

--- Run a normalization operation.
normalize :: Normalize a -> a -> ES String s a
normalize norm x = case evalES (norm x) (0, emptyFM (<)) of
  Left  _  -> failES $ "Normalization failed for: " ++ show x
  Right x' -> returnES x'

--- Normalizes the type variable numbers in the given function.
--- The parameters of the function are always the first types to be
--- renumbered so they are assigned the lowest numbers.
---
--- @param func - the function to normalize
--- @return the normalized function
normFunc :: Normalize (AFuncDecl TypeExpr)
normFunc (AFunc f a v t r) = AFunc f a v <$> normType t <*> normRule r

--- Recursively normalizes type variable numbers in the given type expression.
---
--- @param type - the type expression to normalize
--- @return the normalized type expression
normType :: Normalize TypeExpr
normType (TVar        i) = gets >+= \(n, fm) -> case lookupFM fm i of
  Nothing -> puts (n + 1, addToFM fm i n) >+ returnES (TVar n)
  Just n' -> returnES (TVar n')
normType (TCons   q tys) = TCons q <$> mapES normType tys
normType (FuncType  a b) = FuncType <$> normType a <*> normType b

--- Normalize a rule.
normRule :: Normalize (ARule TypeExpr)
normRule (ARule     ty vs e) = ARule <$> normType ty <*> mapES normSnd vs
                                                     <*> normExpr e
normRule (AExternal ty    s) = flip AExternal s <$> normType ty

--- Normalizes type variable numbers in an expression. The next number
--- to assign and a map from existing variable numbers to newly assigned
--- ones are managed using the state monad.
---
--- @param state - the current state
--- @param expr - the expression to normalize
--- @return the new state and normalized expression inside the state monad
normExpr :: Normalize (AExpr TypeExpr)
normExpr (AVar  t       v) = flip AVar  v  <$> normType t
normExpr (ALit  t       l) = flip ALit  l  <$> normType t
normExpr (AComb t ct f es) = flip AComb ct <$> normType t
                                           <*> normSnd f <*> mapES normExpr es
normExpr (ALet  t    ds e) = ALet <$> normType t <*> mapES normBinding ds
                                                 <*> normExpr e
  where normBinding (v, b) = (\x y -> (x,y)) <$> normSnd  v <*> normExpr b
normExpr (AOr   t     a b) = AOr <$> normType t <*> normExpr a <*> normExpr b
normExpr (ACase t ct e bs) = flip ACase ct <$> normType t <*> normExpr e
                                           <*> mapES normBranch bs
normExpr (AFree  t   vs e) = AFree  <$> normType t <*> mapES normSnd vs
                                    <*> normExpr e
normExpr (ATyped t    e y) = ATyped <$> normType t <*> normExpr e <*> normType y

normSnd :: Normalize (a, TypeExpr)
normSnd (a, ty) = normType ty >+= \ty' -> returnES (a, ty')

--- Normalizes type variable numbers in a branch. State is managed
--- using the state monad, see normExpr for details.
---
--- @param state - the current state
--- @param branch - the branch to normalize
--- @return the new state and normalized branch inside the state monad
normBranch :: Normalize (ABranchExpr TypeExpr)
normBranch (ABranch p e) = ABranch <$> normPattern p <*> normExpr e

normPattern :: Normalize (APattern TypeExpr)
normPattern (APattern  t c vs) = APattern <$> normType t <*> normSnd c
                                          <*> mapES normSnd vs
normPattern (ALPattern t    l) = flip ALPattern l <$> normType t
