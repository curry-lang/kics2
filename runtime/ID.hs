{-# LANGUAGE ExistentialQuantification #-}
-- ---------------------------------------------------------------------------
-- ID module
-- ---------------------------------------------------------------------------
module ID
  ( -- * Constraints
    Constraint (..), Constraints(..), getConstrList
    -- * Decisions
  , Decision (..), defaultDecision, isDefaultDecision
    -- * IDs
  , ID (..), leftID, rightID, narrowID, getKey
  , IDSupply, initSupply, leftSupply, rightSupply, thisID, freeID
    -- * Decision management
  , lookupDecision, lookupID, lookupDecisionID, setDecision, setUnsetDecision
  , nextNIDs, Store (..)
  ) where

import Control.Monad (liftM, when, zipWithM_)

import IDSupply

-- ---------------------------------------------------------------------------
-- Constraint
-- ---------------------------------------------------------------------------

-- |Type to encode constraints for a Choice(s) structure
data Constraint
  -- |Binding of an 'ID' to a 'Decision'
  = ID :=: Decision
  -- |Unsolvable constraint
  | Unsolvable
  -- |Non-deterministic choice between two lists of constraints
  | ConstraintChoice ID [Constraint] [Constraint]
  -- |Non-deterministic choice between a list of lists of constraints
  | ConstraintChoices ID [[Constraint]]
 deriving (Show,Eq)

-- A Value Constraint is used to bind a Value to an id it also contains the
-- structural constraint information that describes the choice to be taken
-- for a given id, a Struct Constraint has only the structural information
data Constraints
  = forall a . ValConstr ID a [Constraint]
  | StructConstr [Constraint]

-- a selector to get the strucural constraint information from a constraint
getConstrList :: Constraints -> [Constraint]
getConstrList (ValConstr _ _ c) = c
getConstrList (StructConstr  c) = c

instance Show Constraints where
  showsPrec _ (ValConstr _ _ c) = showParen True
                                $ showString "ValC "    . shows c
  showsPrec _ (StructConstr  c) = showParen True
                                $ showString "StructC " . shows c

instance Eq Constraints where
 c1 == c2 = getConstrList c1 == getConstrList c2

-- ---------------------------------------------------------------------------
-- Decision
-- ---------------------------------------------------------------------------

-- |Type to encode the decision made for a Choice(s) structure
data Decision
    -- |No decision has been made so far
  = NoDecision
    -- |The left value of an (?) is chosen
  | ChooseLeft
    -- |The right value of an (?) is chosen
  | ChooseRight
    -- |ChooseN consIdx argCnt is the choice for the constructor with the
    --  index consIdx which has argCnt arguments
  | ChooseN Int Int
    -- |a free or narrowed variable is bound to the free variable with the
    --  given id; the bindings of the IDs for the arguments have not been
    --  propagated yet
  | BindTo ID
    -- |A free or narrowed variable is bound to the variable with the given
    --  'ID'; the bindings for the n arguments have also been propagated
  | BoundTo ID Int
    -- |A free variable is lazily bound to an expression by a function
   --   pattern
  | LazyBind [Constraint]
    deriving Show

instance Eq Decision where
  NoDecision  == NoDecision  = True
  ChooseLeft  == ChooseLeft  = True
  ChooseRight == ChooseRight = True
  ChooseN c _ == ChooseN d _ = c == d
  BindTo  _   == BindTo  _   = error "ID.Decision.(==): BindTo"
  BoundTo _ _ == BoundTo _ _ = error "ID.Decision.(==): BoundTo"
  LazyBind _  == LazyBind _  = error "ID.Decision.(==): LazyBind"
  _           == _           = False

-- |Default 'Decision'. The default 'Decision' is provided via a function to
-- break recursive dependencies.
defaultDecision :: Decision
defaultDecision = NoDecision

-- |Is the given 'Decision' the 'defaultDecision'?
isDefaultDecision :: Decision -> Bool
isDefaultDecision NoDecision = True
isDefaultDecision _          = False

-- ---------------------------------------------------------------------------
-- ID
-- ---------------------------------------------------------------------------

-- |Type to identify different Choice structures in a non-deterministic result
data ID
    -- |Identifier for a choice introduced by using of the (?) operator
  = ChoiceID Unique
    -- |Identifier for a choice for a free variable
  | FreeID [Int] IDSupply
    -- |Identifier for a choice for a narrowed variable (free before)
  | NarrowedID [Int] IDSupply
    deriving Eq

instance Show ID where
  show (ChoiceID     i) = "?" ++ show i
  show (FreeID     _ i) = "_x" ++ show i
  show (NarrowedID _ i) = "Narrowed" ++ show i

-- |Retrieve the 'IDSupply' from an 'ID'
supply :: ID -> IDSupply
supply (ChoiceID     _) = error "ID.supply: ChoiceID"
supply (FreeID     _ s) = s
supply (NarrowedID _ s) = s

-- |Construct an 'ID' for a free variable from an 'IDSupply'
freeID :: [Int] -> IDSupply -> ID
freeID = FreeID

-- |Construct an 'ID' for a binary choice from an 'IDSupply'
thisID :: IDSupply -> ID
thisID = ChoiceID . unique

-- |Convert a free or narrowed 'ID' into a narrowed one
narrowID :: ID -> ID
narrowID (ChoiceID _) = error "ID.narrowID: ID"
narrowID (FreeID p s) = NarrowedID p s
narrowID narrowedID   = narrowedID

-- |Retrieve the left child 'ID' from a free 'ID'
leftID :: ID -> ID
leftID  (FreeID _ s) = freeID [] (leftSupply s)
leftID  _            = error "ID.leftID: no FreeID"

-- |Retrieve the right child 'ID' from a free 'ID'
rightID :: ID -> ID
rightID (FreeID _ s) = freeID [] (rightSupply s)
rightID  _           = error "ID.rightID: no FreeID"

getKey :: ID -> Integer
getKey = mkInteger . getUnique

getUnique :: ID -> Unique
getUnique (ChoiceID     u) = u
getUnique (FreeID     _ s) = unique s
getUnique (NarrowedID _ s) = unique s

-- ---------------------------------------------------------------------------
-- Looking up decisions
-- ---------------------------------------------------------------------------

-- |Lookup the 'Decision' an 'ID' ultimately is bound to
lookupDecision :: Store m => ID -> m Decision
lookupDecision i = fst `liftM` lookupDecisionID i

-- |Lookup the 'ID' an 'ID' ultimately is bound to
lookupID :: Store m => ID -> m ID
lookupID i = snd `liftM` lookupDecisionID i

-- |Lookup the 'Decision' and the 'ID' an 'ID' ultimately is bound to
lookupDecisionID :: Store m => ID -> m (Decision, ID)
lookupDecisionID i = getDecisionRaw (getUnique i) >>= unchain
  where
    -- TODO: reactivate shortening of chains as soon as we know how
    --       to do this correct and efficient
    -- For BindTo, we shorten chains of multiple BindTos by directly binding
    -- to the last ID in the chain.
    unchain (BindTo j) = do
      retVal@(c, _lastId) <- lookupDecisionID j
      case c of
        NoDecision    -> return () --shortenTo lastId
        ChooseN _ num -> propagateBind i j num -- lastId num
        LazyBind _    -> return () --shortenTo lastId
        _             -> error $ "ID.lookupDecisionID: " ++ show c
      return retVal
--       where
--         shortenTo lastId = when (j /= lastId) $ do
--           trace $ "shorten " ++ show i ++ " to " ++ show lastId
--           setChoiceRaw i (BindTo lastId)

    -- For BoundTo, the chains should already be shortened since the Choice
    -- "BoundTo j" is only set if the variable j has been set to a "ChooseN"
    -- and therefore could not have been changed in between.
    -- TODO: check if the previous statement is correct
    unchain (BoundTo j num) = do
      retVal@(c, _) <- lookupDecisionID j
      case c of
        NoDecision     -> return ()
        ChooseN _ num' -> checkPropagation i j num num'
        LazyBind _     -> return ()
        _              -> error $ "ID.lookupDecisionID: " ++ show c
      return retVal

    -- For all other choices, there are no chains at all
    unchain c           = return (c, i)

-- ---------------------------------------------------------------------------
-- Setting decisions
-- ---------------------------------------------------------------------------

-- |Set the 'Decision' for the given 'ID'
setDecision :: Store m => ID -> Decision -> m ()
setDecision i c = setDecisionGetChange i c >> return ()

-- Set the given 'Decision' for the given 'ID' and return an action to recover
-- the former 'Decision'
setUnsetDecision :: Store m => ID -> Decision -> m (m ())
setUnsetDecision i c = do
  mChange <- setDecisionGetChange i c
  case mChange of
    Nothing                       -> return (return ())
    Just (oldDecision, changedId) -> return $ case c of
      BindTo _ -> resetFreeVar changedId oldDecision
      _        -> setDecisionRaw (getUnique changedId) oldDecision

-- |Set the 'Decision' for the given 'ID', eventually following a chain and
--  return the ultimately changed 'ID' and its former 'Decision'
setDecisionGetChange :: Store m => ID -> Decision -> m (Maybe (Decision, ID))
-- We do not bind an ID to itself to avoid cycles
setDecisionGetChange i (BindTo j) | supply i == supply j = return Nothing
setDecisionGetChange i c = getDecisionRaw (getUnique i) >>= unchain
  where
  -- BindTo: change the last variable in the chain and propagate the binding
  -- TODO: At the moment the propagation is necessary, but may be removed
  -- later (cf. tests/Unification.curry, goal25)
  unchain (BindTo k)    = do
    retVal <- setDecisionGetChange k c
    case c of
      ChooseN _ num -> propagateBind i k num
      _             -> return ()
    return retVal
  -- BoundTo: change the last variable in the chain
  -- If the new ChooseN has a different propagation number, the old propagation
  -- has to be reset first. Otherwise after a lookup leading to BoundTo
  -- new propagations would be ignored, cf. tests/FunctionPattern.curry, goal2
  unchain (BoundTo k num) = do
    retVal <- setDecisionGetChange k c
    case c of
      ChooseN _ num' -> checkPropagation i k num num'
      _              -> return ()
    return retVal
  unchain oldDecision     = case c of
    BindTo j -> do
      -- Avoid binding i to a variable which is transitively bound to i
      lastId <- lookupID j
      if getKey lastId == getKey i
        then return Nothing
        else setDecisionRaw (getUnique i) c >> return (Just (oldDecision, i))
    _     -> setDecisionRaw (getUnique i) c >> return (Just (oldDecision, i))

-- ---------------------------------------------------------------------------
-- Auxiliary functions
-- ---------------------------------------------------------------------------

checkPropagation :: Store m => ID -> ID -> Int -> Int -> m ()
checkPropagation i j oldNum newNum = when (oldNum /= newNum) $ do
  resetFreeVar i (BindTo j)
  propagateBind i j newNum

-- |Propagate a binding of 'ID' x to 'ID' y for the next cnt independent child
--  'ID's. x as well as y are both expected to be either a free or a narrowed
--  variable
propagateBind :: Store m => ID -> ID -> Int -> m ()
propagateBind x y cnt = do
  -- bind i to j
  setDecisionRaw (getUnique x) (BoundTo y cnt)
  -- propagate the binding to the children
  zipWithM_ (\a b -> setDecision a (BindTo b)) (nextNIDs x cnt) (nextNIDs y cnt)

-- |Reset a free variable to its former 'Decision' and reset its children if
--  the binding has already been propagated
--  TODO: use set/lookupDecisionRef to avoid constructor wrapping
resetFreeVar :: Store m => ID -> Decision -> m ()
resetFreeVar i oldDecision = reset oldDecision i -- (supply i)
  where
  reset c j = getDecisionRaw (getUnique j) >>= propagate c j

  propagate c j (BindTo _)      = setDecisionRaw (getUnique j) c
  propagate c j (BoundTo _ num) = do
    setDecisionRaw (getUnique j) c
    mapM_ (reset NoDecision) $ nextNIDs j num
  propagate _ _ _ = error "ID.resetFreeVar.propagate: no binding"

-- Compute a list of the next n free 'ID's for a given 'ID'
nextNIDs :: ID -> Int -> [ID]
nextNIDs = nextNIDsFromSupply . supply

-- Compute a list of the next n free 'ID's for a given 'IDSupply' s
nextNIDsFromSupply :: IDSupply -> Int -> [ID]
nextNIDsFromSupply s n = map (freeID []) $ nextSupplies n s

-- |Compute the next n independent 'IDSupply's for a given 'IDSupply' s
nextSupplies :: Int -> IDSupply -> [IDSupply]
nextSupplies n s
  | n <  0    = error $ "ID.nextNSupplies: " ++ show n
  | n == 0    = []
  | n == 1    = [leftSupply s]
  | otherwise = nextNSupplies' n s
  where
  nextNSupplies' n' s'
    | n' == 1    = [s']
    | otherwise =  nextNSupplies' (n' - halfn) (leftSupply  s')
                ++ nextNSupplies' halfn        (rightSupply s')
    where halfn = n' `div` 2
