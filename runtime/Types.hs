-- ---------------------------------------------------------------------------
-- This module contains the basic type definitions to represent Curry types
-- in Haskell
-- ---------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types
  ( module ConstStore
  , module ID
  , module Types
  ) where

import ConstStore
import KiCS2Debug
import FailInfo
import ID

-- ---------------------------------------------------------------------------
-- Try structure
-- ---------------------------------------------------------------------------

-- TODO: Reason about pros and cons of reusing Choices for free, narrowed, (?)

-- |Data type to represent non-deterministic values in a generic structure.
--
-- The 'Try' structure is used to provide a type-independent representation
-- of non-deterministic values in the runtime system, e.g. for implementing
-- the search strategies.
data Try a
  = Val a                     -- ^ Value in head normal form (HNF)
  | Fail Cover FailInfo       -- ^ Failure with covering depth and additional information
  | Choice Cover ID a a       -- ^ Binary choice, introduced by the (?) operator
  | Narrowed Cover ID [a]     -- ^ N-ary choice for narrowed variable
  | Free Cover ID [a]         -- ^ N-ary choice for free variable, where
                              --   N corresponds to the number of constructors of
                              --   the underlying type
  | Guard Cover Constraints a -- ^ Constrained value
    deriving Show

-- |Convert a binary choice of type a into one of type 'Try' a
tryChoice :: Cover -> ID -> a -> a -> Try a
tryChoice cd i@(ChoiceID    _  ) = Choice cd i
tryChoice _  _                   = internalError "Basics.tryChoice: no ChoiceID"

-- |Convert a n-ary choice of type a into one of type 'Try' a
tryChoices :: Cover -> ID -> [a] -> Try a
tryChoices cd i@(FreeID          _ _) = Free     cd i
tryChoices cd i@(NarrowedID      _ _) = Narrowed cd i
tryChoices _  i                       = internalError $ "Basics.tryChoices: wrong ID " ++ show i

-- unused because of triviality:

-- tryFail :: Try a
-- tryFail = Fail

-- tryGuard :: Constraints -> a -> Try a
-- tryGuard = Guard

-- tryVal :: a -> Try a
-- tryVal = Val

-- ---------------------------------------------------------------------------
-- Non-determinism
-- ---------------------------------------------------------------------------

-- |Class for types that support nondeterministic values
class NonDet a where
  -- |Construct a binary choice, used by the (?) operator
  choiceCons :: Cover -> ID -> a -> a -> a
  -- |Construct a n-ary choice, used for free variables and narrowing
  choicesCons:: Cover -> ID -> [a] -> a
  -- |Construct a failed computation
  failCons   :: Cover -> FailInfo -> a
  -- |Construct a constrained value
  guardCons  :: Cover -> Constraints -> a -> a
  -- |Convert a value into the generic 'Try' structure
  try        :: a -> Try a
  -- |Apply the adequate function to a non-deterministic value, where each of
  --  the supplied functions handles a different constructor.
  --
  -- /Note:/ This functionality was introduced to render the conversion from
  -- and to the 'Try' structure obsolete. Nonetheless, the performance impact
  -- still is to be analyzed.
  match      :: (Cover -> ID -> a -> a -> b)     -- ^ Binary Choice
             -> (Cover -> ID -> [a] -> b)        -- ^ n-ary Choice for narrowed variable
             -> (Cover -> ID -> [a] -> b)        -- ^ n-ary Choice for free variable
             -> (Cover -> FailInfo -> b)         -- ^ Failure
             -> (Cover -> Constraints -> a -> b) -- ^ Constrained value
             -> (a -> b)                         -- ^ Head Normal Form
             -> a                                -- ^ value to apply the functions to
             -> b

  try = match Choice Narrowed Free Fail Guard Val

  match chc nrwd fr fl grd vl x = case try x of
    Val v             -> vl v
    Fail cd info      -> fl cd info
    Choice cd i x1 x2 -> chc cd i x1 x2
    Narrowed cd i xs  -> nrwd cd i xs
    Free cd i xs      -> fr cd i xs
    Guard cd cs e     -> grd cd cs e

-- |Lift a choice encountered at pattern matching to the result value.
-- The name of this function is misleading because of historical reasons
-- and should be renamed to sth. like "choice"
narrow :: NonDet a => Cover -> ID -> a -> a -> a
narrow cd i@(ChoiceID      _) = choiceCons cd i
narrow _  _                   = internalError "Basics.narrow: no ChoiceID"

-- |Convert an n-ary choice of a free variable into one with a narrowed variable
-- |If the varible is bound in either the local or the global constraint store
-- |the value found in the store is used
narrows :: NonDet b => ConstStore -> Cover -> ID -> (a -> b) -> [a] -> b
narrows cs cd i@(FreeID        p s) f xs
  = lookupWithGlobalCs cs i f $ choicesCons cd (NarrowedID p s) (map f xs)
narrows _  cd i@(NarrowedID      _ _) f xs = choicesCons cd i (map f xs)
narrows _  _    (ChoiceID          _) _ _  = internalError "Types.narrows: ChoiceID"


bindOrNarrow :: Unifiable a => Cover -> ID -> Cover -> ID -> [a] -> [Constraint]
bindOrNarrow cd i d j@(FreeID p s) xs
  | d < cd    = [ConstraintChoices d (NarrowedID p s) (map (bind cd i) xs)]
  | otherwise = [ i :=: BindTo j]
bindOrNarrow _  _ _ j _ = internalError $ "Types.bindOrNarrow: " ++ show j

lazyBindOrNarrow :: Unifiable a => Cover -> ID -> Cover -> ID -> [a] -> [Constraint]
lazyBindOrNarrow cd i d j@(FreeID p s) xs
  | d < cd = [ConstraintChoices d (NarrowedID p s) (map (lazyBind cd i) xs)]
  | otherwise    = [ i :=: BindTo j ]
lazyBindOrNarrow _  _ _ j _ = internalError $ "Types.lazyBindOrNarrow: " ++ show j

-- ---------------------------------------------------------------------------
-- Computation of normal forms
-- ---------------------------------------------------------------------------

-- |Class for types that support the computaton of its normal form (NF) and
--  ground normal form (GNF).
--
-- While NF allows free variables, GNF does not, therefore free variables will
-- be narrowed when computing the GNF.
--
-- The NF/GNF computation is combined with a continuation to be applied to
-- the NF/GNF.
class (NonDet a, Generable a, Show a) => NormalForm a where
  -- |Apply a continuation to the normal form
  ($!!) :: NonDet b => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
  -- |Apply a continuation to the ground normal form
  ($##) :: NonDet b => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
  -- show of constructor
  showCons :: a -> String
  showCons = show
  -- new approach
  -- - 1. argument: the search
  -- - 2. argument: a continuation that is applied to the normal form of v
  -- - 3. argument: A value with a value constructor at the root
  searchNF :: (forall b . NormalForm b => (b -> c) -> b -> c) -> (a -> c) -> a -> c

-- |Auxiliary function to apply the continuation to the normal forms of the
-- two alternatives of a binary choice.
nfChoice :: (NormalForm a, NonDet b)
         => (a -> Cover -> ConstStore -> b) -> Cover -> ID -> a -> a -> Cover -> ConstStore -> b
nfChoice cont d i x1 x2 cd cs = case i of
  ChoiceID      _ -> choiceCons d i ((cont $!! x1) cd cs) ((cont $!! x2) cd cs)
  _               -> internalError "Basics.nfChoice: no ChoiceID"

-- |Auxiliary function to apply the continuation to the normal forms of the
-- n alternatives of a n-ary choice.
nfChoices :: (NormalForm a, NonDet b)
          => (a -> Cover -> ConstStore -> b) -> Cover -> ID -> [a] -> Cover -> ConstStore -> b
nfChoices cont d i xs cd cs = case i of
   ChoiceID _     -> internalError "Basics.nfChoices: ChoiceID"
   FreeID _ _     -> cont (choicesCons d i xs) cd cs
   NarrowedID _ _ -> choicesCons d i (map (\x -> (cont $!! x) cd cs) xs)

-- |Auxiliary function to apply the continuation to the ground normal forms of
-- the two alternatives of a binary choice.
gnfChoice :: (NormalForm a, NonDet b)
          => (a -> Cover -> ConstStore -> b) -> Cover -> ID -> a -> a -> Cover -> ConstStore -> b
gnfChoice cont d i x1 x2 cd cs = case i of
  ChoiceID _ -> choiceCons d i ((cont $## x1) cd cs) ((cont $## x2) cd cs)
  _          -> internalError "Basics.gnfChoice: no ChoiceID"

-- |Auxiliary function to apply the continuation to the ground normal forms of
-- the n alternatives of a n-ary choice.
gnfChoices :: (NormalForm a, NonDet b)
           => (a -> Cover -> ConstStore -> b) -> Cover -> ID -> [a] -> Cover -> ConstStore -> b
gnfChoices cont d i xs cd cs = narrows cs d i (\x -> (cont $## x) cd cs) xs

-- ---------------------------------------------------------------------------
-- Generator function for free variables
-- ---------------------------------------------------------------------------

-- |Class for types that support generator functions to represent free
-- variables.
class NonDet a => Generable a where
  -- |Generate a free variable for the given 'IDSupply'
  generate :: IDSupply -> Cover -> a

-- ---------------------------------------------------------------------------
-- Unification
-- ---------------------------------------------------------------------------

-- Class for data that supports unification
class (NormalForm a) => Unifiable a where
  -- |Unification on constructor-rooted terms
  (=.=)    :: a -> a -> Cover -> ConstStore -> C_Bool
  -- |Lazy unification on constructor-rooted terms,
  --  used for functional patterns
  (=.<=)   :: a -> a -> Cover -> ConstStore -> C_Bool
  -- |Bind a free variable to a value
  bind     :: Cover -> ID -> a -> [Constraint]
  -- |Lazily bind a free variable to a value
  lazyBind :: Cover -> ID -> a -> [Constraint]

-- |Unification on general terms
(=:=) :: Unifiable a => a -> a -> Cover -> ConstStore -> C_Bool
#ifdef TRY
(=:=) = unifyTry
#else
(=:=) = unifyMatch
#endif

unifyMatch :: Unifiable a => a -> a -> Cover -> ConstStore -> C_Bool
unifyMatch x y cd cs = match uniChoice uniNarrowed uniFree failCons uniGuard uniVal x
  where
  uniChoice d i x1 x2 = checkFail (choiceCons d i ((x1 =:= y) cd cs) ((x2 =:= y) cd cs)) y
  uniNarrowed d i xs  = checkFail (choicesCons d i (map (\x' -> (x' =:= y) cd cs) xs)) y
  uniFree cdi i xs    = lookupCs cs i (\xval -> (xval =:= y) cd cs)
                          (if cdi < cd then (unifyMatch (narrows cs cdi i id xs) y cd cs) else (bindTo cs y))
                                      -- TODO: use global cs
    where
    bindTo cs' y' = match bindChoice bindNarrowed bindFree failCons bindGuard bindVal y'
      where
      bindChoice   cdj j y1 y2 = choiceCons  cdj j (bindTo cs' y1) (bindTo cs' y2)
      bindNarrowed cdj j ys    = choicesCons cdj j (map (bindTo cs') ys)
      bindFree     cdj j ys    = lookupCs cs j (bindTo cs') $
               if cdj < cd
               then unifyMatch x (narrows cs cdj j id ys) cd cs
-- if a variable is bound to itself, no constraint is generated at all
-- in order to prevent a loop caused by the constraint store optimization
               else if i == j then C_True
                              else guardCons cd (ValConstr i y' [i :=: BindTo j]) C_True
      bindGuard cdj c    = guardCons cdj c . (bindTo $! c `addCs` cs')
      bindVal v          = bindToVal i v cd cs'

  uniGuard cdx c e    = checkFail (guardCons cdx c ((e =:= y) cd $! c `addCs` cs)) y
  uniVal v          = uniWith cs y
    where
    uniWith cs' y' = match univChoice univNarrowed univFree failCons univGuard univVal y'
      where
      univChoice d j y1 y2   = choiceCons d  j (uniWith cs' y1) (uniWith cs' y2)
      univNarrowed d j ys    = choicesCons d j (map (uniWith cs') ys)
      univFree d j ys        = lookupCs cs j (uniWith cs')
                    (if d < cd then uniWith cs' (narrows cs' cd j id ys)
                    else (bindToVal j v cd cs'))
      univGuard d c          = guardCons d c . (uniWith $! c `addCs` cs')
      univVal w            = (v =.= w) cd cs'
  checkFail e = match (\_ _ _ _ -> e) const3 const3 failCons const3 (const e)
    where const3 _ _ _ = e

unifyTry :: Unifiable a => a -> a -> Cover -> ConstStore -> C_Bool
unifyTry xVal yVal cd csVal = unify (try xVal) (try yVal) csVal -- 1. compute HNF hx, hy
  where
  -- failure
  unify (Fail d info) _              _ = failCons d info
  unify _              (Fail d info) _ = failCons d info
  -- binary choice
  unify (Choice d i x1 x2) hy cs = choiceCons d i (unify (try x1) hy cs)
                                                  (unify (try x2) hy cs)
  unify hx (Choice d j y1 y2) cs = choiceCons d j (unify hx (try y1) cs)
                                                  (unify hx (try y2) cs)
  -- n-ary choice
  unify (Narrowed d i xs) hy cs = choicesCons d i (map (\x -> unify (try x) hy cs) xs)
  unify hx (Narrowed d j ys) cs = choicesCons d j (map (\y -> unify hx (try y) cs) ys)
  -- constrained value
  unify (Guard d c x) hy cs = guardCons d c (unify (try x) hy $! c `addCs` cs)
  unify hx (Guard d c y) cs = guardCons d c (unify hx (try y) $! c `addCs` cs)
  -- constructor-rooted terms
  unify (Val x) (Val y) cs = (x =.= y) cd cs
  -- two free variables
  unify hx@(Free cdi i xs) hy@(Free cdj j nfy) cs = lookupCs cs i
    (\x -> unify (try x) hy cs)
    (lookupCs cs j (\y -> unify hx (try y) cs)
                   (if cdi < cd
                    then unify (try (narrows cs cdi i id xs)) hy cs
                    else (if cdj < cd
                           then unify hx (try (narrows cs cdj j id nfy)) cs
-- if a variable is bound to itself, no constraint is generated at all
-- in order to prevent a loop caused by the constraint store optimization
                           else (if i == j
                                  then C_True
                                  else guardCons cdi (ValConstr i nfy [i :=: BindTo j]) C_True))))
  -- one free variable and one value
  unify (Free cdi i xs) hy@(Val y) cs = lookupCs cs i
    (\x -> unify (try x) hy cs)
    (if cdi < cd then unify (try (narrows cs cdi i id xs)) hy cs
                 else bindToVal i y cd cs)
  -- one free variable and one value
  unify hx@(Val x) (Free cdj j ys) cs = lookupCs cs j
    (\y -> unify hx (try y) cs)
    (if cdj < cd then unify hx (try (narrows cs cdj j id ys)) cs
                 else bindToVal j x cd cs)

bindToVal :: Unifiable a => ID -> a -> Cover -> ConstStore -> C_Bool
#ifdef STRICT_VAL_BIND
bindToVal i v cd cs = ((\w _ -> constrain cd i w) $!! v) cs
#else
bindToVal i v cd _ =           constrain cd i v
#endif

constrain :: Unifiable a => Cover -> ID -> a -> C_Bool
constrain cd i v = guardCons cd (ValConstr i v (bind cd i v)) C_True

  -- TODO2: Occurs check?

-- Lazy unification on general terms, used for function patterns
(=:<=) :: Unifiable a => a -> a -> Cover -> ConstStore -> C_Bool
#ifdef TRY
(=:<=) = lazyTry
#else
(=:<=) = lazyMatch
#endif

lazyMatch :: Unifiable a => a -> a -> Cover -> ConstStore -> C_Bool
lazyMatch x y cd cs = match uniChoice uniNarrowed uniFree failCons uniGuard uniVal x
  where
  -- binary choice
  uniChoice d i x1 x2 = choiceCons d i ((x1 =:<= y) cd cs) ((x2 =:<= y) cd cs)
  -- n-ary choice
  uniNarrowed d i xs  = choicesCons d i (map (\z -> (z =:<= y) cd cs) xs)
  -- constrained value
  uniGuard d c e      = guardCons d c ((e =:<= y) cd $! c `addCs` cs)
  -- free variable
  uniFree d i xs   = lookupCs cs i (\xVal -> (xVal =:<= y) cd cs)
                       (if d < cd
                        then (narrows cs d i id xs =:<= y) cd cs
                        else guardCons d (StructConstr [i :=: LazyBind (lazyBind cd i y)]) C_True)
  -- constructor-rooted term
  uniVal vx = unifyWith cs y
    where
    unifyWith cs' = match uniyChoice uniyNarrowed uniyFree failCons uniyGuard uniyVal
      where
      uniyChoice d j y1 y2  = choiceCons d j (unifyWith cs' y1) (unifyWith cs' y2)
      uniyNarrowed d j ys   = choicesCons d j (map (unifyWith cs') ys)
      uniyGuard d c         = guardCons d c . (unifyWith $! c `addCs` cs')
      uniyVal vy            = (vx =.<= vy) cd cs'
      uniyFree d j ys       =
         lookupCs cs' j (unifyWith cs')
          (if d < cd
           then unifyWith cs' (narrows cs' d j id ys)
           else guardCons d (StructConstr [head (lazyBind cd j vx)])
                                          (choicesCons d (narrowID j) (map (\z -> ((vx =:<= z)  cd cs')) ys)))

lazyTry :: Unifiable a => a -> a -> Cover -> ConstStore -> C_Bool
lazyTry x y cd cs = case try x of
  -- failure
  Fail     d info    -> failCons d info
  -- binary choice
  Choice   d i x1 x2 -> choiceCons d i ((x1 =:<= y) cd cs) ((x2 =:<= y) cd cs)
  -- n-ary choice
  Narrowed d i xs    -> choicesCons d i (map (\z -> (z =:<= y) cd cs) xs)
  -- constrained value
  Guard    d c e     -> guardCons d c ((e =:<= y) cd $! c `addCs` cs)
  -- free variable
  Free     d i xs    -> lookupCs cs i (\xVal -> (xVal =:<= y) cd cs)
                        (if d < cd
                          then (narrows cs d i id xs =:<= y) cd cs
                          else guardCons d (StructConstr [i :=: LazyBind (lazyBind cd i y)]) C_True)
  Val        vx      -> lazyVal cs y
    where
    lazyVal cs' y' = case try y' of
      -- failure
      Fail     d info    -> failCons d info
      -- binary choice
      Choice   d j y1 y2 -> choiceCons d j (lazyVal cs' y1) (lazyVal cs' y2)
      -- n-ary choice
      Narrowed d j ys    -> choicesCons d j (map (lazyVal cs') ys)
      -- constrained value
      Guard    d c e     -> guardCons d c $ (lazyVal $! c `addCs` cs') e
      -- free variable
      Free     d j ys    -> lookupCs cs' j (lazyVal cs')
                            (if d < cd
                            then lazyVal cs' (narrows cs' d j id ys)
                            else guardCons d (StructConstr [head (lazyBind cd j vx)])
                                             (choicesCons d (narrowID j) (map (\z -> ((vx =:<= z)  cd cs')) ys)))
      Val        vy      -> (vx =.<= vy) cd cs'

-- ---------------------------------------------------------------------------
-- Curry types
-- ---------------------------------------------------------------------------

-- Class for Curry types
class (Show a, Read a, NonDet a, Generable a, NormalForm a, Unifiable a)
      => Curry a

-- ---------------------------------------------------------------------------
-- Conversion between Curry and Haskell data types
-- ---------------------------------------------------------------------------

class ConvertCurryHaskell ctype htype where -- needs MultiParamTypeClasses
  fromCurry :: ctype -> htype
  toCurry   :: htype -> ctype

instance (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb)
  => ConvertCurryHaskell (ca -> cb) (ha -> hb) where
  fromCurry f = fromCurry . f . toCurry
  toCurry   f = toCurry   . f . fromCurry


-- ---------------------------------------------------------------------------
-- Auxiliaries for Show and Read
-- ---------------------------------------------------------------------------

showsChoice :: Show a => Int -> Cover -> ID -> a -> a -> ShowS
showsChoice d _ i@(ChoiceID _) x1 x2
  = showString "(?" . shows i . showChar ' '
  . showsPrec d x1 . showChar ' ' . showsPrec d x2
  . showChar ')'
showsChoice _ _ _ _ _ = internalError "showsChoice: No ChoiceID"

showsChoices :: Show a => Int -> Cover -> ID -> [a] -> ShowS
showsChoices _ _   (ChoiceID _)     _  = internalError "showsChoices: ChoiceID"
showsChoices _ _ i@(FreeID _ _)     _  = shows i
showsChoices d _ i@(NarrowedID _ _) xs
  = showString "[?" . shows i
  . foldr (.) id (zipWith showNarrowing [(0 :: Int) ..] xs)
  . showChar ']'
  where showNarrowing n x = showString ", " . shows n
                          . showString "->" . showsPrec d x

showsGuard :: (Show a, Show b) => Int -> Cover -> a -> b -> ShowS
showsGuard d _ c e = showsPrec d c . showString " &> " . showsPrec d e

-- |Read a possibly qualified name
readQualified :: String -> String -> ReadS ()
readQualified mdl name r =
 let lexname = lex r in
     [((),s)  | (name',s)  <- lexname, name' == name]
  ++ [((),s3) | (mod',s1)  <- lexname
              , mod' == mdl
              , (".", s2)   <- lex s1
              , (name', s3) <- lex s2
              , name' == name]

-- ---------------------------------------------------------------------------
-- Boolean type
-- ---------------------------------------------------------------------------

data C_Bool
  = C_False
  | C_True
  | Choice_C_Bool Cover ID C_Bool C_Bool
  | Choices_C_Bool Cover ID [C_Bool]
  | Fail_C_Bool Cover FailInfo
  | Guard_C_Bool Cover Constraints C_Bool

instance Show C_Bool where
  showsPrec d (Choice_C_Bool cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Bool cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Bool cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Bool cd info) = showChar '!'
  showsPrec _ C_False = showString "False"
  showsPrec _ C_True = showString "True"

instance Read C_Bool where
  readsPrec _ s = readParen False (\r -> [(C_False, r0) | (_, r0) <-
    readQualified "Prelude" "False" r]) s ++ readParen False (\r -> [(C_True
    , r0) | (_, r0) <- readQualified "Prelude" "True" r]) s

instance NonDet C_Bool where
  choiceCons = Choice_C_Bool
  choicesCons = Choices_C_Bool
  failCons = Fail_C_Bool
  guardCons = Guard_C_Bool
  try (Choice_C_Bool cd i x y) = tryChoice cd i x y
  try (Choices_C_Bool cd i xs) = tryChoices cd i xs
  try (Fail_C_Bool cd info) = Fail cd info
  try (Guard_C_Bool cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Bool cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Bool cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Bool cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Bool cd i _) = error
    ("Prelude.Bool.match: Choices with ChoiceID " ++ show i)
  match _ _ _ f _ _ (Fail_C_Bool cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Bool cd c e) = f cd c e
  match _ _ _ _ _ f x = f x

instance Generable C_Bool where
  generate s c = Choices_C_Bool c (freeID [0, 0] s) [C_False, C_True]

instance NormalForm C_Bool where
  ($!!) cont C_False d cs = cont C_False d cs
  ($!!) cont C_True d cs = cont C_True d cs
  ($!!) cont (Choice_C_Bool cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Bool cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Bool cd c e) d cs = guardCons cd c (($!!) cont e d (addCs
    c cs))
  ($!!) _ (Fail_C_Bool cd info) _ _ = failCons cd info
  ($##) cont C_False d cs = cont C_False d cs
  ($##) cont C_True d cs = cont C_True d cs
  ($##) cont (Choice_C_Bool cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Bool cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Bool cd c e) d cs = guardCons cd c (($##) cont e d (addCs
    c cs))
  ($##) _ (Fail_C_Bool cd info) _ _ = failCons cd info
  showCons C_False = "Prelude.False"
  showCons C_True = "Prelude.True"
  showCons x = error ("Prelude.Bool.showCons: no constructor: " ++ show x)
  searchNF _ cont C_False = cont C_False
  searchNF _ cont C_True = cont C_True
  searchNF _ _ x = error ("Prelude.Bool.searchNF: no constructor: " ++ show x)

instance Unifiable C_Bool where
  (=.=) C_False C_False d cs = C_True
  (=.=) C_True C_True d cs = C_True
  (=.=) a b cd _ = Fail_C_Bool cd (unificationFail (showCons a) (showCons b))
  (=.<=) C_False C_False d cs = C_True
  (=.<=) C_True C_True d cs = C_True
  (=.<=) a b cd _ = Fail_C_Bool cd (unificationFail (showCons a) (showCons
    b))
  bind cd i C_False = (i :=: ChooseN 0 0) : concat []
  bind cd i C_True = (i :=: ChooseN 1 0) : concat []
  bind d i (Choice_C_Bool cd j x y) = [ConstraintChoice cd j (bind d i x) (bind
    d i y)]
  bind d i (Choices_C_Bool cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Bool cd j@(NarrowedID _ _) xs) = [ConstraintChoices cd j
    (map (bind d i) xs)]
  bind _ _ (Choices_C_Bool cd i _) = error
    ("Prelude.Bool.bind: Choices with ChoiceID: " ++ show i)
  bind _ _ (Fail_C_Bool cd info) = [Unsolvable info]
  bind d i (Guard_C_Bool cd c e) = getConstrList c ++ bind d i e
  lazyBind cd i C_False = (i :=: ChooseN 0 0) : []
  lazyBind cd i C_True = (i :=: ChooseN 1 0) : []
  lazyBind d i (Choice_C_Bool cd j x y) = [ConstraintChoice cd j (lazyBind d i
    x) (lazyBind d i y)]
  lazyBind d i (Choices_C_Bool cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j
    xs
  lazyBind d i (Choices_C_Bool cd j@(NarrowedID _ _) xs) = [ConstraintChoices cd
    j (map (lazyBind d i) xs)]
  lazyBind _ _ (Choices_C_Bool cd i _) = error
    ("Prelude.Bool.lazyBind: Choices with ChoiceID: " ++ show i)
  lazyBind _ _ (Fail_C_Bool cd info) = [Unsolvable info]
  lazyBind d i (Guard_C_Bool cd c e) = getConstrList c ++ [i :=: LazyBind
    (lazyBind d i e)]

instance Curry C_Bool

-- ---------------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------------

-- Higher Order functions
instance Show (a -> b) where
  show _ = "<<function>>"

instance Read (a -> b) where
  readsPrec = internalError "read for function is undefined"

instance NonDet b => NonDet (a -> b) where
  choiceCons  cd i f g = \ x -> choiceCons  cd i (f x) (g x)
  choicesCons cd i fs  = \ x -> choicesCons cd i (map ($x) fs)
  failCons    cd info  = \ _ -> failCons cd info
  guardCons   cd  c f  = \ x -> guardCons cd c (f x)
  try                  = Val

instance NonDet b => Generable (a -> b) where
  generate = internalError "generate for function is undefined"

instance NonDet b => NormalForm (a -> b) where
  cont $!! f = cont f
  cont $## f = cont f
  searchNF _ cont f = cont f

instance NonDet b => Unifiable (a -> b) where
  (=.=)    = internalError "(=.=) for function is undefined"
  (=.<=)   = internalError "(=.<=) for function is undefined"
  bind     = internalError "bind for function is undefined"
  lazyBind = internalError "lazyBind for function is undefined"

instance NonDet b => Curry (a -> b)
