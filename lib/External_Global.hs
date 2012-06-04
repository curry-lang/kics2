import Data.IORef
import System.IO
import System.Directory(doesFileExist)
import System.IO.Unsafe
import qualified Curry_Prelude as CP

-- Implementation of Globals in Curry. We use Haskell's IORefs for temporary
-- globals where Curry values are stored in the IORefs
data C_Global a
     = Choice_C_Global Cover ID (C_Global a) (C_Global a)
     | Choices_C_Global Cover ID ([C_Global a])
     | Fail_C_Global Cover FailInfo
     | Guard_C_Global Cover Constraints (C_Global a)
     | C_Global_Temp (IORef a)  -- a temporary global
     | C_Global_Pers String     -- a persistent global with a given (file) name

instance Show (C_Global a) where
  show = error "ERROR: no show for Global"

instance Read (C_Global a) where
  readsPrec = error "ERROR: no read for Global"

instance NonDet (C_Global a) where
  choiceCons = Choice_C_Global
  choicesCons = Choices_C_Global
  failCons = Fail_C_Global
  guardCons = Guard_C_Global
  try (Choice_C_Global cd i x y) = tryChoice cd i x y
  try (Choices_C_Global cd i xs) = tryChoices cd i xs
  try (Fail_C_Global cd info) = Fail cd info
  try (Guard_C_Global cd c e) = Guard cd c e
  try x = Val x
  match choiceF _ _ _ _ _ (Choice_C_Global cd i x y) = choiceF cd i x y
  match _ narrF _ _ _ _   (Choices_C_Global cd i@(NarrowedID _ _) xs) = narrF cd i xs
  match _ _ freeF _ _ _   (Choices_C_Global cd i@(FreeID _ _) xs)     = freeF cd i xs
  match _ _ _ failF _ _   (Fail_C_Global cd info) = failF cd info
  match _ _ _ _ guardF _  (Guard_C_Global cd c e) = guardF cd c e
  match _ _ _ _ _ valF    x                    = valF x

instance Generable (C_Global a) where
  generate _ = error "ERROR: no generator for Global"

instance NormalForm (C_Global a) where
  ($!!) cont g@(C_Global_Temp _)          cs = cont g cs
  ($!!) cont g@(C_Global_Pers _)          cs = cont g cs
  ($!!) cont (Choice_C_Global cd i g1 g2) cs = nfChoice cont cd i g1 g2 cs
  ($!!) cont (Choices_C_Global cd i gs)   cs = nfChoices cont cd i gs cs
  ($!!) cont (Guard_C_Global cd c g)      cs = guardCons cd c ((cont $!! g) (addCs c cs))
  ($!!) _    (Fail_C_Global cd info)      cs = failCons cd info
  ($##) cont g@(C_Global_Temp _)          cs = cont g cs
  ($##) cont g@(C_Global_Pers _)          cs = cont g cs
  ($##) cont (Choice_C_Global cd i g1 g2) cs = gnfChoice cont cd i g1 g2 cs
  ($##) cont (Choices_C_Global cd i gs)   cs = gnfChoices cont cd i gs cs
  ($##) cont (Guard_C_Global cd c g)      cs = guardCons cd c ((cont $## g) (addCs c cs))
  ($##) _    (Fail_C_Global cd info)      cs = failCons cd info
  ($!<) cont (Choice_C_Global cd i g1 g2)    = nfChoiceIO cont cd i g1 g2
  ($!<) cont (Choices_C_Global cd i gs)      = nfChoicesIO cont cd i gs
  ($!<) cont x                               = cont x
  searchNF _ cont g@(C_Global_Temp _)        = cont g
  searchNF _ cont g@(C_Global_Pers _)        = cont g

instance Unifiable (C_Global a) where
  (=.=) (C_Global_Temp ref1) (C_Global_Temp ref2) _
    | ref1 == ref2 = C_Success
  (=.=) (C_Global_Pers f1) (C_Global_Pers f2) _
    | f1 == f2  = C_Success
  (=.=) _ _ _ = Fail_C_Success 0 defFailInfo
  (=.<=) = (=.=)
  bind i (Choice_C_Global cd j l r) = [(ConstraintChoice cd j (bind i l) (bind i r))]
  bind i (Choices_C_Global cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Global cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Fail_C_Global cd info) = [Unsolvable info]
  bind i (Guard_C_Global _ cs e) = (getConstrList cs) ++ (bind i e)
  lazyBind i (Choice_C_Global cd j l r) = [(ConstraintChoice cd j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Global cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Global cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Fail_C_Global cd info) = [Unsolvable info]
  lazyBind i (Guard_C_Global _ cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind i e)))]

instance CP.Curry a => CP.Curry (C_Global a) where
  (=?=) = error "(==) is undefined for Globals"
  (<?=) = error "(<=) is undefined for Globals"

instance Coverable (C_Global a) where
  cover (Choice_C_Global cd i x y) = Choice_C_Global (incCover cd) i (cover x) (cover y) 
  cover (Choices_C_Global cd i xs) = Choices_C_Global (incCover cd) i (map cover xs)
  cover (Fail_C_Global cd info)    = Fail_C_Global (incCover cd) info 
  cover (Guard_C_Global cd cs x)   = Guard_C_Global (incCover cd) cs (cover x)     
  cover x@(C_Global_Temp _)        = x
  cover x@(C_Global_Pers _)        = x


external_d_C_global :: CP.Curry a => a -> C_GlobalSpec -> ConstStore -> C_Global a
external_d_C_global val C_Temporary _ = ref `seq` (C_Global_Temp ref)
  where ref = unsafePerformIO (newIORef val)
external_d_C_global val (C_Persistent cname) _ =
  let name = fromCurry cname :: String
   in unsafePerformIO (initGlobalFile name >> return (C_Global_Pers name))
 where initGlobalFile name = do
         ex <- doesFileExist name
         if ex then return ()
               else writeFile name (show val++"\n")

external_d_C_prim_readGlobal :: CP.Curry a => C_Global a -> ConstStore -> CP.C_IO a
external_d_C_prim_readGlobal (C_Global_Temp  ref) _ = fromIO (readIORef ref)
external_d_C_prim_readGlobal (C_Global_Pers name) _ = fromIO $
  do h <- openFile name ReadMode
     s <- hGetLine h
     hClose h
     return (read s)

external_d_C_prim_writeGlobal :: CP.Curry a => C_Global a -> a
                                            -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_writeGlobal (C_Global_Temp ref) val _ =
  toCurry (writeIORef ref val)
external_d_C_prim_writeGlobal (C_Global_Pers name) val _ =
  toCurry (writeFile name (show val ++ "\n"))
