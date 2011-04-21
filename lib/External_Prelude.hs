{-# LANGUAGE MagicHash, MultiParamTypeClasses #-}

import GHC.IO.Exception (IOException (..))

-- ATTENTION: Do not introduce line breaks in import declarations as these
-- are not recognized!
import GHC.Exts (Int (I#), Int#, (==#), (/=#), (<#), (>#), (<=#), (+#), (-#), (*#), quotInt#, remInt#, negateInt#)
import GHC.Exts (Float (F#), Float#, eqFloat#, leFloat#, negateFloat#)
import GHC.Exts (Char (C#), Char#, eqChar#, leChar#, ord#, chr#)

-- ---------------------------------------------------------------------------
-- Externals
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Curry types
-- ---------------------------------------------------------------------------

-- Class for curry types
class (Show a, Read a, NonDet a, Generable a, NormalForm a, Unifiable a) => Curry a where
  (=?=) :: a -> a -> C_Bool
  (=?=) = error "(=?=) is undefined"

  (<?=) :: a -> a -> C_Bool
  (<?=) = error "(<?=) is undefined"

instance Curry (PrimData a) where
  (=?=) = error "(=?=) is undefined for primitive data"
  (<?=) = error "(<?=) is undefined for primitive data"

-- BEGIN GENERATED FROM PrimTypes.curry
instance Curry C_Success where
  (=?=) (Choice_C_Success i x y) z = narrow i (x =?= z) (y =?= z)
  (=?=) (Choices_C_Success i xs) y = narrows i (map ((=?= y)) xs)
  (=?=) (Guard_C_Success c x) y = guardCons c (x =?= y)
  (=?=) Fail_C_Success _ = failCons
  (=?=) z (Choice_C_Success i x y) = narrow i (z =?= x) (z =?= y)
  (=?=) y (Choices_C_Success i xs) = narrows i (map ((y =?=)) xs)
  (=?=) y (Guard_C_Success c x) = guardCons c (y =?= x)
  (=?=) _ Fail_C_Success = failCons
  (=?=) C_Success C_Success = C_True
  (<?=) (Choice_C_Success i x y) z = narrow i (x <?= z) (y <?= z)
  (<?=) (Choices_C_Success i xs) y = narrows i (map ((<?= y)) xs)
  (<?=) (Guard_C_Success c x) y = guardCons c (x <?= y)
  (<?=) Fail_C_Success _ = failCons
  (<?=) z (Choice_C_Success i x y) = narrow i (z <?= x) (z <?= y)
  (<?=) y (Choices_C_Success i xs) = narrows i (map ((y <?=)) xs)
  (<?=) y (Guard_C_Success c x) = guardCons c (y <?= x)
  (<?=) _ Fail_C_Success = failCons
  (<?=) C_Success C_Success = C_True
-- END GENERATED FROM PrimTypes.curry

instance (Curry t0,Curry t1) => Curry (Func t0 t1) where

instance Curry t0 => Curry (C_IO t0) where

instance Curry (a -> b) where

-- ---------------------------------------------------------------------------
-- Int
-- ---------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data C_Int
     = C_Int Int#
     | C_CurryInt C_Integer
     | Choice_C_Int ID C_Int C_Int
     | Choices_C_Int ID ([C_Int])
     | Fail_C_Int
     | Guard_C_Int ([Constraint]) C_Int

instance Show C_Int where
  showsPrec d (Choice_C_Int i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Int i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Int c e) = showsGuard d c e
  showsPrec _ Fail_C_Int = showChar '!'
  showsPrec d (C_Int x1) = shows (I# x1)
  showsPrec d (C_CurryInt x1) = (\x -> shows (I# (curryint2primint x))) $!! x1

instance Read C_Int where
  readsPrec d s = map readInt (readsPrec d s) where readInt (I# i, s) = (C_Int i, s)

instance NonDet C_Int where
  choiceCons = Choice_C_Int
  choicesCons = Choices_C_Int
  failCons = Fail_C_Int
  guardCons = Guard_C_Int
  try (Choice_C_Int i x y) = tryChoice i x y
  try (Choices_C_Int i xs) = tryChoices i xs
  try Fail_C_Int = Fail
  try (Guard_C_Int c e) = Guard c e
  try x = Val x

instance Generable C_Int where
--   generate s = C_CurryInt (generate s)
  generate s = Choices_C_Int (freeID s) [C_CurryInt (generate (leftSupply s))]

instance NormalForm C_Int where
  ($!!) cont x@(C_Int _) = cont x
  ($!!) cont (C_CurryInt x1) = (\y1 -> cont (C_CurryInt y1)) $!! x1
  ($!!) cont (Choice_C_Int i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_Int i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_Int c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_Int = failCons
  ($##) cont x@(C_Int _) = cont x
  ($##) cont (C_CurryInt x1) = (\y1 -> cont (C_CurryInt y1)) $## x1
  ($##) cont (Choice_C_Int i x y) = gnfChoice cont i x y
  ($##) cont (Choices_C_Int i xs) = gnfChoices cont i xs
  ($##) cont (Guard_C_Int c x) = guardCons c (cont $## x)
  ($##) _ Fail_C_Int = failCons
  ($!<) cont (C_CurryInt x1) = (\y1 -> cont (C_CurryInt y1)) $!< x1
  ($!<) cont (Choice_C_Int i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Int i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont x@(C_Int _) = cont x
  searchNF search cont (C_CurryInt x1) = search (\y1 -> cont (C_CurryInt y1)) x1

instance Unifiable C_Int where
  (=.=) (C_Int      x1) (C_Int      y1) = if (x1 ==# y1) then C_Success else Fail_C_Success
  (=.=) (C_Int      x1) (C_CurryInt y1) = (primint2curryint x1) =:= y1
  (=.=) (C_CurryInt x1) (C_Int      y1) = x1 =:= (primint2curryint y1)
  (=.=) (C_CurryInt x1) (C_CurryInt y1) = x1 =:= y1
  (=.=) _ _ = Fail_C_Success
  (=.<=) (C_Int      x1) (C_Int      y1) = if (x1 ==# y1) then C_Success else Fail_C_Success
  (=.<=) (C_Int      x1) (C_CurryInt y1) = (primint2curryint x1) =:<= y1
  (=.<=) (C_CurryInt x1) (C_Int      y1) = x1 =:<= (primint2curryint y1)
  (=.<=) (C_CurryInt x1) (C_CurryInt y1) = x1 =:<= y1
  (=.<=) _ _ = Fail_C_Success
--   bind i (C_Int x2) = bind i (primint2curryint x2)
--   bind i (C_CurryInt x2) = bind i x2
  bind i (C_Int      x2) = (i :=: ChooseN 0 1) : bind (leftID i) (primint2curryint x2)
  bind i (C_CurryInt x2) = (i :=: ChooseN 0 1) : bind (leftID i) x2
  bind i (Choice_C_Int j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Int j@(FreeID _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Int j@(Narrowed _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Int = [Unsolvable]
  bind i (Guard_C_Int cs e) = cs ++ (bind i e)
  lazyBind i (C_Int      x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) (primint2curryint x2))]
  lazyBind i (C_CurryInt x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) x2)]
  lazyBind i (Choice_C_Int j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Int j@(FreeID _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Int j@(Narrowed _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Int = [Unsolvable]
  lazyBind i (Guard_C_Int cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry_Prelude.Curry C_Int where
  (=?=) (Choice_C_Int i x y) z = narrow i (x =?= z) (y =?= z)
  (=?=) (Choices_C_Int i xs) y = narrows i (map ((=?= y)) xs)
  (=?=) (Guard_C_Int c x) y = guardCons c (x =?= y)
  (=?=) Fail_C_Int _ = failCons
  (=?=) z (Choice_C_Int i x y) = narrow i (z =?= x) (z =?= y)
  (=?=) y (Choices_C_Int i xs) = narrows i (map ((y =?=)) xs)
  (=?=) y (Guard_C_Int c x) = guardCons c (y =?= x)
  (=?=) _ Fail_C_Int = failCons
  (=?=) (C_Int      x1) (C_Int      y1) = toCurry (x1 ==# y1)
  (=?=) (C_Int      x1) (C_CurryInt y1) = (primint2curryint x1) =?= y1
  (=?=) (C_CurryInt x1) (C_Int      y1) = x1 =?= (primint2curryint y1)
  (=?=) (C_CurryInt x1) (C_CurryInt y1) = x1 =?= y1
  (<?=) (Choice_C_Int i x y) z = narrow i (x <?= z) (y <?= z)
  (<?=) (Choices_C_Int i xs) y = narrows i (map ((<?= y)) xs)
  (<?=) (Guard_C_Int c x) y = guardCons c (x <?= y)
  (<?=) Fail_C_Int _ = failCons
  (<?=) z (Choice_C_Int i x y) = narrow i (z <?= x) (z <?= y)
  (<?=) y (Choices_C_Int i xs) = narrows i (map ((y <?=)) xs)
  (<?=) y (Guard_C_Int c x) = guardCons c (y <?= x)
  (<?=) _ Fail_C_Int = failCons
  (<?=) (C_Int      x1) (C_Int      y1) = toCurry (x1 <=# y1)
  (<?=) (C_Int      x1) (C_CurryInt y1) = (primint2curryint x1) `d_C_lteqInteger` y1
  (<?=) (C_CurryInt x1) (C_Int      y1) = x1 `d_C_lteqInteger` (primint2curryint y1)
  (<?=) (C_CurryInt x1) (C_CurryInt y1) = x1 `d_C_lteqInteger` y1
-- END GENERATED FROM PrimTypes.curry

primint2curryint :: Int# -> C_Integer
primint2curryint n
  | n <#  0#  = C_Neg (primint2currynat (negateInt# n))
  | n ==# 0#  = C_Zero
  | otherwise = C_Pos (primint2currynat n)

primint2currynat :: Int# -> C_Nat
primint2currynat n
  | n ==# 1#                = C_IHi
  | (n `remInt#` 2#) ==# 0# = C_O (primint2currynat (n `quotInt#` 2#))
  | otherwise               = C_I (primint2currynat (n `quotInt#` 2#))

currynat2primint :: C_Nat -> Int#
currynat2primint C_IHi   = 1#
currynat2primint (C_O n) = 2# *# currynat2primint n
currynat2primint (C_I n) = 2# *# currynat2primint n +# 1#
currynat2primint _       = error "Prelude.currynat2primint: no ground term"

curryint2primint :: C_Integer -> Int#
curryint2primint C_Zero    = 0#
curryint2primint (C_Pos n) = currynat2primint n
curryint2primint (C_Neg n) = negateInt# (currynat2primint n)
curryint2primint x         = error "Prelude.curryint2primint: no ground term"




--   match valF _ _ _ _ v@(C_Int _) = valF v
--   match _ fail _ _ _ Fail_C_Int  = fail
--   match _ _ choiceF _ _ (Choice_C_Int i@(ID _) x y) = choiceF i x y
--   match _ _ _ freeF _ (Choice_C_Int i@(FreeID _) x y) = freeF i x y
--   match _ _ _ _ guardF (Guard_C_Int c x) = guardF c x

-- ---------------------------------------------------------------------------
-- Float
-- ---------------------------------------------------------------------------
data C_Float
     = C_Float Float#
     | Choice_C_Float ID C_Float C_Float
     | Choices_C_Float ID ([C_Float])
     | Fail_C_Float
     | Guard_C_Float ([Constraint]) C_Float

instance Show C_Float where
  showsPrec d (Choice_C_Float i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Float i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Float c e) = showsGuard d c e
  showsPrec d Fail_C_Float = showChar '!'
  showsPrec d (C_Float x1) = shows (F# x1)

instance Read C_Float where
  readsPrec d s = map readFloat (readsPrec d s) where readFloat (F# f, s) = (C_Float f, s)

instance NonDet C_Float where
  choiceCons = Choice_C_Float
  choicesCons = Choices_C_Float
  failCons = Fail_C_Float
  guardCons = Guard_C_Float
  try (Choice_C_Float i x y) = tryChoice i x y
  try (Choices_C_Float i xs) = tryChoices i xs
  try Fail_C_Float = Fail
  try (Guard_C_Float c e) = Guard c e
  try x = Val x

instance Generable C_Float where
  generate _ = error "No generator for C_Float"

instance NormalForm C_Float where
  ($!!) cont x@(C_Float _) = cont x
  ($!!) cont (Choice_C_Float i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_Float i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_Float c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_Float = failCons
  ($##) cont x@(C_Float _) = cont x
  ($##) cont (Choice_C_Float i x y) = gnfChoice cont i x y
  ($##) cont (Choices_C_Float i xs) = gnfChoices cont i xs
  ($##) cont (Guard_C_Float c x) = guardCons c (cont $## x)
  ($##) _ Fail_C_Float = failCons
  ($!<) cont (Choice_C_Float i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Float i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont x@(C_Float _) = cont x

instance Unifiable C_Float where
  (=.=) _ _ = Fail_C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i (Choice_C_Float j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Float j@(FreeID _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Float j@(Narrowed _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Float = [Unsolvable]
  bind i (Guard_C_Float cs e) = cs ++ (bind i e)
  lazyBind i (Choice_C_Float j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Float j@(FreeID _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Float j@(Narrowed _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Float = [Unsolvable]
  lazyBind i (Guard_C_Float cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry C_Float where
  (=?=) (Choice_C_Float i x y) z = narrow i (x =?= z) (y =?= z)
  (=?=) (Choices_C_Float i xs) y = narrows i (map ((=?= y)) xs)
  (=?=) (Guard_C_Float c x) y = guardCons c (x =?= y)
  (=?=) Fail_C_Float _ = failCons
  (=?=) z (Choice_C_Float i x y) = narrow i (z =?= x) (z =?= y)
  (=?=) y (Choices_C_Float i xs) = narrows i (map ((y =?=)) xs)
  (=?=) y (Guard_C_Float c x) = guardCons c (y =?= x)
  (=?=) _ Fail_C_Float = failCons
  (=?=) (C_Float x1) (C_Float y1) = toCurry (x1 `eqFloat#` y1)
  (<?=) (Choice_C_Float i x y) z = narrow i (x <?= z) (y <?= z)
  (<?=) (Choices_C_Float i xs) y = narrows i (map ((<?= y)) xs)
  (<?=) (Guard_C_Float c x) y = guardCons c (x <?= y)
  (<?=) Fail_C_Float _ = failCons
  (<?=) z (Choice_C_Float i x y) = narrow i (z <?= x) (z <?= y)
  (<?=) y (Choices_C_Float i xs) = narrows i (map ((y <?=)) xs)
  (<?=) y (Guard_C_Float c x) = guardCons c (y <?= x)
  (<?=) _ Fail_C_Float = failCons
  (<?=) (C_Float x1) (C_Float y1) = toCurry (x1 `leFloat#` y1)

-- ---------------------------------------------------------------------------
-- Char
-- ---------------------------------------------------------------------------
data C_Char
     = C_Char Char#
     | Choice_C_Char ID C_Char C_Char
     | Choices_C_Char ID ([C_Char])
     | Fail_C_Char
     | Guard_C_Char ([Constraint]) C_Char

instance Show C_Char where
  showsPrec d (Choice_C_Char i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Char i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Char c e) = showsGuard d c e
  showsPrec d Fail_C_Char = showChar '!'
  showsPrec d (C_Char x1) = showString (show (C# x1))

  showList cs = showList (map (\(C_Char c) -> (C# c)) cs)

instance Read C_Char where
  readsPrec d s = map readChar (readsPrec d s) where readChar (C# c, s) = (C_Char c, s)

  readList s = map readString (readList s) where readString (cs, s) = (map (\(C# c) -> C_Char c) cs, s)

instance NonDet C_Char where
  choiceCons = Choice_C_Char
  choicesCons = Choices_C_Char
  failCons = Fail_C_Char
  guardCons = Guard_C_Char
  try (Choice_C_Char i x y) = tryChoice i x y
  try (Choices_C_Char i xs) = tryChoices i xs
  try Fail_C_Char = Fail
  try (Guard_C_Char c e) = Guard c e
  try x = Val x

instance Generable C_Char where
  generate _ = error "No generator for C_Char"

instance NormalForm C_Char where
  ($!!) cont x@(C_Char _) = cont x
  ($!!) cont (Choice_C_Char i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_Char i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_Char c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_Char = failCons
  ($##) cont x@(C_Char _) = cont x
  ($##) cont (Choice_C_Char i x y) = gnfChoice cont i x y
  ($##) cont (Choices_C_Char i xs) = gnfChoices cont i xs
  ($##) cont (Guard_C_Char c x) = guardCons c (cont $## x)
  ($##) _ Fail_C_Char = failCons
  ($!<) cont (Choice_C_Char i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Char i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont c@(C_Char _) = cont c

instance Unifiable C_Char where
  (=.=) _ _ = Fail_C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i (Choice_C_Char j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Char j@(FreeID _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Char j@(Narrowed _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Char = [Unsolvable]
  bind i (Guard_C_Char cs e) = cs ++ (bind i e)
  lazyBind i (Choice_C_Char j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Char j@(FreeID _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Char j@(Narrowed _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Char = [Unsolvable]
  lazyBind i (Guard_C_Char cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry C_Char where
  (=?=) (Choice_C_Char i x y) z = narrow i (x =?= z) (y =?= z)
  (=?=) (Choices_C_Char i xs) y = narrows i (map ((=?= y)) xs)
  (=?=) (Guard_C_Char c x) y = guardCons c (x =?= y)
  (=?=) Fail_C_Char _ = failCons
  (=?=) z (Choice_C_Char i x y) = narrow i (z =?= x) (z =?= y)
  (=?=) y (Choices_C_Char i xs) = narrows i (map ((y =?=)) xs)
  (=?=) y (Guard_C_Char c x) = guardCons c (y =?= x)
  (=?=) _ Fail_C_Char = failCons
  (=?=) (C_Char x1) (C_Char y1) = toCurry (x1 `eqChar#` y1)
  (<?=) (Choice_C_Char i x y) z = narrow i (x <?= z) (y <?= z)
  (<?=) (Choices_C_Char i xs) y = narrows i (map ((<?= y)) xs)
  (<?=) (Guard_C_Char c x) y = guardCons c (x <?= y)
  (<?=) Fail_C_Char _ = failCons
  (<?=) z (Choice_C_Char i x y) = narrow i (z <?= x) (z <?= y)
  (<?=) y (Choices_C_Char i xs) = narrows i (map ((y <?=)) xs)
  (<?=) y (Guard_C_Char c x) = guardCons c (y <?= x)
  (<?=) _ Fail_C_Char = failCons
  (<?=) (C_Char x1) (C_Char y1) = toCurry (x1 `leChar#` y1)

-- ---------------------------------------------------------------------------
-- Conversion from and to primitive Haskell types
-- ---------------------------------------------------------------------------

instance ConvertCurryHaskell C_Int Int where
  toCurry (I# i) = C_Int i

  fromCurry (C_Int i) = I# i
  fromCurry _         = error "Int data with no ground term"

instance ConvertCurryHaskell C_Int Integer where
  toCurry i = int2C_Int (fromInteger i)

  fromCurry (C_Int i) = toInteger (I# i)
  fromCurry _         = error "Int data with no ground term"

int2C_Int :: Int -> C_Int
int2C_Int (I# c) = C_Int c

instance ConvertCurryHaskell C_Float Float where
  toCurry (F# f) = C_Float f

  fromCurry (C_Float f) = F# f
  fromCurry _           = error "Float data with no ground term"

instance ConvertCurryHaskell C_Char Char where
  toCurry (C# c) = C_Char c

  fromCurry (C_Char c) = C# c
  fromCurry _          = error "Char data with no ground term"

instance (ConvertCurryHaskell ct ht) =>
         ConvertCurryHaskell (OP_List ct) [ht] where
  toCurry []     = OP_List
  toCurry (c:cs) = OP_Cons (toCurry c) (toCurry cs)

  fromCurry OP_List        = []
  fromCurry (OP_Cons c cs) = fromCurry c : fromCurry cs
  fromCurry _              = error "List data with no ground term"

instance ConvertCurryHaskell C_Bool Bool where
  toCurry True  = C_True
  toCurry False = C_False

  fromCurry C_True  = True
  fromCurry C_False = False
  fromCurry _       = error "Float data with no ground term"

instance ConvertCurryHaskell OP_Unit () where
  toCurry ()  = OP_Unit

  fromCurry OP_Unit = ()
  fromCurry _       = error "Unit data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2) =>
         ConvertCurryHaskell (OP_Tuple2 ct1 ct2) (ht1,ht2) where
  toCurry (x1,x2)  = OP_Tuple2 (toCurry x1) (toCurry x2)

  fromCurry (OP_Tuple2 x1 x2) = (fromCurry x1, fromCurry x2)
  fromCurry _       = error "Pair data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2,
          ConvertCurryHaskell ct3 ht3) =>
         ConvertCurryHaskell (OP_Tuple3 ct1 ct2 ct3) (ht1,ht2,ht3) where
  toCurry (x1,x2,x3)  = OP_Tuple3 (toCurry x1) (toCurry x2) (toCurry x3)

  fromCurry (OP_Tuple3 x1 x2 x3) = (fromCurry x1, fromCurry x2, fromCurry x3)
  fromCurry _       = error "Tuple3 data with no ground term occurred"

instance ConvertCurryHaskell ct ht =>
         ConvertCurryHaskell (C_Maybe ct) (Maybe ht) where
  toCurry Nothing  = C_Nothing
  toCurry (Just x) = C_Just (toCurry x)

  fromCurry C_Nothing  = Nothing
  fromCurry (C_Just x) = Just (fromCurry x)
  fromCurry _          = error "Maybe data with no ground term occurred"

--fromOrdering :: Ordering -> C_Ordering
--fromOrdering LT = C_LT
--fromOrdering EQ = C_EQ
--fromOrdering GT = C_GT


-- ---------------------------------------------------------------------------
-- Auxiliary operations for showing lists
-- ---------------------------------------------------------------------------

showsPrec4CurryList :: Show a => Int -> OP_List a -> ShowS
showsPrec4CurryList d cl =
  if isStandardCurryList cl
  then showsPrec d (clist2hlist cl)
  else showChar '(' . showsPrecRaw d cl . showChar ')'
 where
  isStandardCurryList OP_List = True
  isStandardCurryList (OP_Cons _ xs) = isStandardCurryList xs
  isStandardCurryList _ = False

  clist2hlist OP_List = []
  clist2hlist (OP_Cons x xs) = x : clist2hlist xs

  showsPrecRaw d (Choice_OP_List i x y) = showsChoice d i x y
  showsPrecRaw d (Choices_OP_List i xs) = showsChoices d i xs
  showsPrecRaw d (Guard_OP_List c e) = showsGuard d c e
  showsPrecRaw d Fail_OP_List = showChar '!'
  showsPrecRaw d OP_List = showString "[]"
  showsPrecRaw d (OP_Cons x xs) =
    showParen (d > 5) (showsPrec 6 x . showChar ':' . showsPrecRaw 5 xs)


--- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

-- External DFO
-- -------------

external_d_C_ensureNotFree :: Curry a => a -> a
external_d_C_ensureNotFree x =
  case try x of
    Choice i a b -> choiceCons i (external_d_C_ensureNotFree a)
                                 (external_d_C_ensureNotFree b)
    Choices i xs -> choicesCons i (map external_d_C_ensureNotFree xs)
    Frees i xs   -> narrows i (map external_d_C_ensureNotFree xs)
    -- TODO : reason about the case when there is a constraint
    --        for the free variable e
    Guard c e    -> guardCons c (external_d_C_ensureNotFree e)
    _            -> x

external_d_C_prim_error :: C_String -> a
external_d_C_prim_error s = error (fromCurry s)

external_d_C_failed :: NonDet a => a
external_d_C_failed = failCons

external_d_OP_eq_eq :: Curry a => a -> a -> C_Bool
external_d_OP_eq_eq  = (=?=)

external_d_OP_lt_eq :: Curry a => a -> a -> C_Bool
external_d_OP_lt_eq = (<?=)

external_d_C_prim_ord :: C_Char -> C_Int
external_d_C_prim_ord (C_Char c) = C_Int (ord# c)

external_d_C_prim_chr :: C_Int -> C_Char
external_d_C_prim_chr (C_Int i) = C_Char (chr# i)

external_d_OP_plus :: C_Int -> C_Int -> C_Int
external_d_OP_plus (C_Int      x) (C_Int      y) = C_Int (x +# y)
external_d_OP_plus (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_OP_plus_hash` y)
external_d_OP_plus (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_OP_plus_hash` (primint2curryint y))
external_d_OP_plus (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_OP_plus_hash` y)
external_d_OP_plus x y = (\a -> (\b -> (a `external_d_OP_plus` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_OP_minus :: C_Int -> C_Int -> C_Int
external_d_OP_minus (C_Int      x) (C_Int      y) = C_Int (x -# y)
external_d_OP_minus (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_OP_minus_hash` y)
external_d_OP_minus (C_CurryInt x) (C_Int y)      = C_CurryInt (x `d_OP_minus_hash` (primint2curryint y))
external_d_OP_minus (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_OP_minus_hash` y)
external_d_OP_minus x y = (\a -> (\b -> (a `external_d_OP_minus` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_OP_star :: C_Int -> C_Int -> C_Int
external_d_OP_star (C_Int      x) (C_Int      y) = C_Int (x *# y)
external_d_OP_star (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_OP_star_hash` y)
external_d_OP_star (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_OP_star_hash` (primint2curryint y))
external_d_OP_star (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_OP_star_hash` y)
external_d_OP_star x y = (\a -> (\b -> (a `external_d_OP_star` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_C_quot :: C_Int -> C_Int -> C_Int
external_d_C_quot (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `quotInt#` y)
external_d_C_quot (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_C_quotInteger` y)
external_d_C_quot (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_C_quotInteger` (primint2curryint y))
external_d_C_quot (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_C_quotInteger` y)
external_d_C_quot x y = (\a -> (\b -> (a `external_d_C_quot` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_C_rem :: C_Int -> C_Int -> C_Int
external_d_C_rem (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `remInt#` y)
external_d_C_rem (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_C_remInteger` y)
external_d_C_rem (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_C_remInteger` (primint2curryint y))
external_d_C_rem (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_C_remInteger` y)
external_d_C_rem x y = (\a -> (\b -> (a `external_d_C_rem` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_C_quotRem :: C_Int -> C_Int -> OP_Tuple2 C_Int C_Int
external_d_C_quotRem (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_OP_Tuple2
  | otherwise = OP_Tuple2 (C_Int (x `quotInt#` y)) (C_Int (x `remInt#` y))
external_d_C_quotRem (C_Int      x) (C_CurryInt y) = mkIntTuple `d_dollar_bang` ((primint2curryint x) `d_C_quotRemInteger` y)
external_d_C_quotRem (C_CurryInt x) (C_Int      y) = mkIntTuple `d_dollar_bang` (x `d_C_quotRemInteger` (primint2curryint y))
external_d_C_quotRem (C_CurryInt x) (C_CurryInt y) = mkIntTuple `d_dollar_bang` (x `d_C_quotRemInteger` y)
external_d_C_quotRem x y = (\a -> (\b -> (a `external_d_C_quotRem` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

-- ---------------------------------------------------------------------------
-- PrimOps taken from GHC.Base
-- ---------------------------------------------------------------------------
divInt# :: Int# -> Int# -> Int#
x# `divInt#` y#
        -- Be careful NOT to overflow if we do any additional arithmetic
        -- on the arguments...  the following  previous version of this
        -- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    | (x# ># 0#) && (y# <# 0#) = ((x# -# 1#) `quotInt#` y#) -# 1#
    | (x# <# 0#) && (y# ># 0#) = ((x# +# 1#) `quotInt#` y#) -# 1#
    | otherwise                = x# `quotInt#` y#

modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
    | (x# ># 0#) && (y# <# 0#) ||
      (x# <# 0#) && (y# ># 0#)    = if r# /=# 0# then r# +# y# else 0#
    | otherwise                   = r#
    where
    !r# = x# `remInt#` y#

external_d_C_div :: C_Int -> C_Int -> C_Int
external_d_C_div (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `divInt#` y)
external_d_C_div (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_C_divInteger` y)
external_d_C_div (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_C_divInteger` (primint2curryint y))
external_d_C_div (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_C_divInteger` y)
external_d_C_div x y = (\a -> (\b -> (a `external_d_C_div` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_C_mod :: C_Int -> C_Int -> C_Int
external_d_C_mod (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `modInt#` y)
external_d_C_mod (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_C_modInteger` y)
external_d_C_mod (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_C_modInteger` (primint2curryint y))
external_d_C_mod (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_C_modInteger` y)
external_d_C_mod x y = (\a -> (\b -> (a `external_d_C_mod` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

-- TODO: $! instead of $#?
external_d_C_divMod :: C_Int -> C_Int -> OP_Tuple2 C_Int C_Int
external_d_C_divMod (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_OP_Tuple2
  | otherwise = OP_Tuple2 (C_Int (x `divInt#` y)) (C_Int (x `modInt#` y))
external_d_C_divMod (C_Int      x) (C_CurryInt y) = mkIntTuple `d_OP_dollar_hash` ((primint2curryint x) `d_C_divModInteger` y)
external_d_C_divMod (C_CurryInt x) (C_Int      y) = mkIntTuple `d_OP_dollar_hash` (x `d_C_divModInteger` (primint2curryint y))
external_d_C_divMod (C_CurryInt x) (C_CurryInt y) = mkIntTuple `d_OP_dollar_hash` (x `d_C_divModInteger` y)
external_d_C_divMod x y = (\a -> (\b -> (a `external_d_C_divMod` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

mkIntTuple :: OP_Tuple2 C_Integer C_Integer -> OP_Tuple2 C_Int C_Int
mkIntTuple (OP_Tuple2 d m) = OP_Tuple2 (C_CurryInt d) (C_CurryInt m)

external_d_C_negateFloat :: C_Float -> C_Float
external_d_C_negateFloat (C_Float x) = C_Float (negateFloat# x)
external_d_C_negateFloat x = external_d_C_negateFloat `d_OP_dollar_hash` x

external_d_OP_eq_colon_eq :: Unifiable a => a -> a -> C_Success
external_d_OP_eq_colon_eq = (=:=)

external_d_C_success :: C_Success
external_d_C_success = C_Success

external_d_OP_ampersand :: C_Success -> C_Success -> C_Success
external_d_OP_ampersand = (&)

external_d_C_return :: a -> C_IO a
external_d_C_return a = fromIO (return a)

external_d_C_prim_putChar :: C_Char -> C_IO OP_Unit
external_d_C_prim_putChar = fromHaskellIO1 putChar

external_d_C_getChar :: C_IO C_Char
external_d_C_getChar = fromHaskellIO0 getChar

external_d_C_prim_readFile :: C_String -> C_IO C_String
external_d_C_prim_readFile = fromHaskellIO1 readFile

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_writeFile :: C_String -> C_String -> C_IO OP_Unit
external_d_C_prim_writeFile = fromHaskellIO2 writeFile

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_appendFile :: C_String -> C_String -> C_IO OP_Unit
external_d_C_prim_appendFile = fromHaskellIO2 appendFile

external_d_C_catchFail :: C_IO a -> C_IO a -> C_IO a
external_d_C_catchFail act err = fromIO $ catch (toIO act) handle
  where handle ioErr = print ioErr >> (toIO err)

external_d_C_prim_show :: Show a => a -> C_String
external_d_C_prim_show a = toCurry (show a)

external_d_C_cond :: Curry a => C_Success -> a -> a
external_d_C_cond succ a = const a `d_dollar_bang` succ

external_d_OP_eq_colon_lt_eq :: Curry a => a -> a -> C_Success
external_d_OP_eq_colon_lt_eq = (=:<=)

external_d_OP_eq_colon_lt_lt_eq :: Curry a => a -> a -> C_Success
external_d_OP_eq_colon_lt_lt_eq = error "external_d_OP_eq_colon_lt_lt_eq"

-- External ND
-- -----------

external_nd_OP_qmark :: NonDet a => a -> a -> IDSupply -> a
external_nd_OP_qmark x y ids = let i = thisID ids in i `seq` choiceCons i x y

-- External HO
-- -----------

external_d_OP_dollar_bang :: (NonDet a, NonDet b) => (a -> b) -> a -> b
external_d_OP_dollar_bang = d_dollar_bang

external_nd_OP_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> b
external_nd_OP_dollar_bang = nd_dollar_bang

external_d_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => (a -> b) -> a -> b
external_d_OP_dollar_bang_bang = ($!!)

external_nd_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> b
external_nd_OP_dollar_bang_bang f x s = (\y -> nd_apply f y s) $!! x

external_d_OP_dollar_hash_hash :: (NormalForm a, NonDet b) => (a -> b) -> a -> b
external_d_OP_dollar_hash_hash = ($##)

external_nd_OP_dollar_hash_hash :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> b
external_nd_OP_dollar_hash_hash f x s = (\y -> nd_apply f y s) $## x

external_d_C_apply :: (a -> b) -> a -> b
external_d_C_apply = d_apply

external_nd_C_apply :: NonDet b => Func a b -> a -> IDSupply -> b
external_nd_C_apply = nd_apply

external_d_C_catch :: C_IO a -> (C_IOError -> C_IO a) -> C_IO a
external_d_C_catch act cont = fromIO $ catch (toIO act) handle where
  handle = toIO . cont . C_IOError . toCurry . ioe_description

external_nd_C_catch :: C_IO a -> Func C_IOError (C_IO a) -> IDSupply -> C_IO a
external_nd_C_catch act cont s = C_IO $ catch (toIO act) handle where
  handle e = toIO (nd_apply cont (C_IOError (toCurry (ioe_description e))) s)

-- TODO: Support non-deterministic IO ?
external_d_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> (t0 -> C_IO t1) -> C_IO t1
external_d_OP_gt_gt_eq m f = fromIO $ (toIO m) >>= toIO . f

-- TODO: Support non-deterministic IO ?
external_nd_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> Func t0 (C_IO t1) -> IDSupply -> C_IO t1
external_nd_OP_gt_gt_eq m f s = fromIO $ (toIO m) >>= \x -> toIO (nd_apply f x s)

-- Encapsulated search
-- -------------------

-- external_d_C_try :: (a -> Success) -> [a -> Success]
external_d_C_try = error "external_dho_C_try"

-- external_nd_C_try :: Func a Success -> [Func a Success]
external_nd_C_try = error "external_ndho_C_try"