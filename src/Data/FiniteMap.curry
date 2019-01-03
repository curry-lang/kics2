-----------------------------------------------------------------------------
--- *IMPORTANT NOTE*: This library is deprecated and included only
--- for compatibility with older programs. Use library `Data.Map` instead!
---
--- A finite map is an efficient purely functional data structure
--- to store a mapping from keys to values.
--- In order to store the mapping efficiently, an irreflexive(!) order predicate
--- has to be given, i.e., the order predicate `le` should not satisfy
--- `(le x x)` for some key `x`.
---
--- Example: To store a mapping from `Int -> String`, the finite map needs
--- a Boolean predicate like `(<)`.
--- This version was ported from a corresponding Haskell library
---
--- @author Frank Huch, Bernd Brassel
--- @version January 2019
-----------------------------------------------------------------------------

module Data.FiniteMap (
        FM,                -- abstract type

        emptyFM,
        unitFM,
        listToFM,

        addToFM,
        addToFM_C,
        addListToFM,
        addListToFM_C,
        delFromFM,
        delListFromFM,
        splitFM,

        plusFM,
        plusFM_C,
        minusFM,
        intersectFM,
        intersectFM_C,

        foldFM,
        mapFM,
        filterFM,

        sizeFM,
        eqFM,
        isEmptyFM,
        elemFM,
        lookupFM,
        lookupWithDefaultFM,
        keyOrder,

        fmToList,
        keysFM,
        eltsFM,
        fmSortBy,

        minFM,maxFM,updFM, fmToListPreOrder,

        showFM, readFM
    ) where

import Maybe
import ReadShowTerm (readQTerm, showQTerm)

--- order predicates are boolean
type LeKey key = key -> key -> Bool

-----------------------------------------------
--        BUILDING finite maps
-----------------------------------------------

--- The empty finite map.
--- @param le an irreflexive order predicate on the keys.
--- @result an empty finite map
emptyFM :: (LeKey key) -> FM key _
emptyFM le = FM le EmptyFM

--- Construct a finite map with only a single element.
--- @param le an irreflexive order predicate on the keys.
--- @param key key of
--- @param elt the single element to form
--- @result a finite map with only a single element
unitFM :: (LeKey key) -> key -> elt -> FM key elt
unitFM le key elt = FM le (unitFM' key elt)

unitFM' :: key -> elt -> FiniteMap key elt
unitFM' key elt = BranchFM key elt 1 EmptyFM EmptyFM


--- Builts a finite map from given list of tuples (key,element).
--- For multiple occurences of key, the last corresponding
--- element of the list is taken.
--- @param le an irreflexive order predicate on the keys.
listToFM :: Eq key => (LeKey key) -> [(key,elt)] -> FM key elt
listToFM le = addListToFM (emptyFM le)

-----------------------------------------------
--        ADDING AND DELETING
-----------------------------------------------

--- Throws away any previous binding and stores the new one given.

addToFM :: Eq key => FM key elt -> key -> elt  -> FM key elt
addToFM (FM le fm) key elt = FM le (addToFM' le fm key elt)

addToFM' :: Eq key => (LeKey key) -> FiniteMap key elt -> key -> elt
         -> FiniteMap key elt
addToFM' le fm key elt = addToFM_C' le (\ _ new -> new) fm key elt

addToFM_C' :: Eq key => (LeKey key) -> (elt -> elt -> elt)
           -> FiniteMap key elt -> key -> elt -> FiniteMap key elt
addToFM_C' _ _ EmptyFM key elt = unitFM' key elt
addToFM_C' le combiner (BranchFM key elt size fm_l fm_r) new_key new_elt
  = if le new_key key
    then mkBalBranch key elt (addToFM_C' le combiner fm_l new_key new_elt) fm_r
    else
      if new_key==key
      then BranchFM new_key (combiner elt new_elt) size fm_l fm_r
      else mkBalBranch key elt fm_l (addToFM_C' le combiner fm_r new_key new_elt)


--- Throws away any previous bindings and stores the new ones given.
--- The items are added starting with the first one in the list
addListToFM :: Eq key => FM key elt -> [(key,elt)] -> FM key elt
addListToFM (FM le fm) key_elt_pairs =
  FM le (addListToFM' le fm key_elt_pairs)

addListToFM' :: Eq key => (LeKey key) -> FiniteMap key elt
             -> [(key, elt)] -> FiniteMap key elt
addListToFM' le fm key_elt_pairs =
  addListToFM_C' le (\ _ new -> new) fm key_elt_pairs

addListToFM_C' :: Eq key => (LeKey key) -> (elt -> elt -> elt)
               -> FiniteMap key elt -> [(key, elt)] -> FiniteMap key elt
addListToFM_C' le combiner fm key_elt_pairs
  = foldl add fm key_elt_pairs        -- foldl adds from the left
  where
    add fmap (key,elt) = addToFM_C' le combiner fmap key elt


--- Instead of throwing away the old binding,
--- addToFM_C combines the new element with the old one.
--- @param combiner a function combining to elements
--- @param fm a finite map
--- @param key the key of the elements to be combined
--- @param elt the new element
--- @result a modified finite map
addToFM_C :: Eq key => (elt -> elt -> elt) -> FM key elt -> key -> elt
                                 -> FM key elt
addToFM_C combiner (FM le fm) key elt =
  FM le (addToFM_C' le combiner fm key elt)

--- Combine with a list of tuples (key,element), cf. addToFM_C
addListToFM_C :: Eq key => (elt -> elt -> elt) -> FM key elt -> [(key,elt)]
              -> FM key elt
addListToFM_C combiner (FM le fm) key_elt_pairs =
  FM le (addListToFM_C' le combiner fm key_elt_pairs)

--- Deletes key from finite map.
--- Deletion doesn't complain if you try to delete something
--- which isn't there
delFromFM :: Eq key => FM key elt -> key   -> FM key elt
delFromFM (FM le fm) del_key = FM le (delFromFM' le fm del_key)

delFromFM' :: Eq key => (LeKey key) -> FiniteMap key elt -> key
           -> FiniteMap key elt
delFromFM' _ EmptyFM _ = EmptyFM
delFromFM' le (BranchFM key elt _ fm_l fm_r) del_key
  = if le del_key key
    then mkBalBranch key elt (delFromFM' le fm_l del_key) fm_r
    else
      if del_key==key
        then glueBal le fm_l fm_r
        else mkBalBranch key elt fm_l (delFromFM' le fm_r del_key)

--- Deletes a list of keys from finite map.
--- Deletion doesn't complain if you try to delete something
--- which isn't there
delListFromFM :: Eq key => FM key elt -> [key] -> FM key elt
delListFromFM (FM le fm) keys = FM le (foldl (delFromFM' le) fm keys)

--- Applies a function to element bound to given key.
updFM :: Eq a => FM a b -> a -> (b -> b) -> FM a b
updFM (FM lt fm) i f = FM lt (upd fm)
  where
    upd EmptyFM                          =  EmptyFM
    upd (BranchFM k x h l r)
            | i == k     =  BranchFM k (f x) h l r
            | lt i k     =  BranchFM k x h (upd l) r
            | otherwise  =  BranchFM k x h l (upd r)

--- Combines delFrom and lookup.
splitFM :: Eq a => FM a b -> a -> Maybe (FM a b,(a,b))
splitFM g v = maybe Nothing (\x->Just (delFromFM g v,(v,x))) (lookupFM g v)

-------------------------------------------------
-- COMBINING finite maps
-------------------------------------------------

--- Efficiently add key/element mappings of two maps into a single one.
--- Bindings in right argument shadow those in the left
plusFM :: Eq key => FM key elt -> FM key elt -> FM key elt
plusFM (FM le1 fm1) (FM _ fm2) = FM le1 (plusFM' le1 fm1 fm2)

plusFM' :: Eq key => (LeKey key)
        -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
plusFM' _  EmptyFM fm2 = fm2
plusFM' _  (BranchFM split_key1 elt1 s1 left1 right1) EmptyFM =
  (BranchFM split_key1 elt1 s1 left1 right1)
plusFM' le (BranchFM split_key1 elt1 s1 left1 right1)
           (BranchFM split_key elt2 _ left right)
  = mkVBalBranch le split_key elt2 (plusFM' le lts left) (plusFM' le gts right)
  where
    fm1 = BranchFM split_key1 elt1 s1 left1 right1
    lts     = splitLT le fm1 split_key
    gts     = splitGT le fm1 split_key

--- Efficiently combine key/element mappings of two maps into a single one,
--- cf. addToFM_C
plusFM_C :: Eq key => (elt -> elt -> elt)
         -> FM key elt -> FM key elt -> FM key elt
plusFM_C combiner (FM le1 fm1) (FM _ fm2) =
  FM le1 (plusFM_C' le1 combiner fm1 fm2)

plusFM_C' :: Eq key => LeKey key -> (elt -> elt -> elt)
          -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
plusFM_C' _  _        EmptyFM fm2 = fm2
plusFM_C' _  _        (BranchFM split_key1 elt1 s1 left1 right1) EmptyFM =
          BranchFM split_key1 elt1 s1 left1 right1
plusFM_C' le combiner (BranchFM split_key1 elt1 s1 left1 right1)
                      (BranchFM split_key elt2 _ left right)
  = mkVBalBranch le split_key new_elt
                 (plusFM_C' le combiner lts left)
                 (plusFM_C' le combiner gts right)
  where
    fm1 = BranchFM split_key1 elt1 s1 left1 right1
    lts     = splitLT le fm1 split_key
    gts     = splitGT le fm1 split_key
    new_elt = case lookupFM' le fm1 split_key of
                Nothing    -> elt2
                Just elt1' -> combiner elt1' elt2

--- (minusFM a1 a2) deletes from a1 any bindings which are bound in a2
minusFM :: Eq key => FM key elt -> FM key elt -> FM key elt
minusFM (FM le1 fm1) (FM _ fm2) = FM le1 (minusFM' le1 fm1 fm2)

minusFM' :: Eq key => (LeKey key)
         -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
minusFM' _  EmptyFM _ = EmptyFM
minusFM' _  (BranchFM split_key1 elt1 s1 left1 right1) EmptyFM =
  BranchFM split_key1 elt1 s1 left1 right1
minusFM' le (BranchFM split_key1 elt1 s1 left1 right1)
            (BranchFM split_key _ _ left right)
  = glueVBal le (minusFM' le lts left) (minusFM' le gts right)
       -- The two can be way different, so we need glueVBal
  where
    fm1 = BranchFM split_key1 elt1 s1 left1 right1
    lts = splitLT le fm1 split_key  -- NB gt and lt, so the equal ones
    gts = splitGT le fm1 split_key  -- are not in either.

--- Filters only those keys that are bound in both of the given maps.
--- The elements will be taken from the second map.
intersectFM :: Eq key => FM key elt -> FM key elt -> FM key elt
intersectFM (FM le1 fm1) (FM _ fm2) = FM le1 (intersectFM' le1 fm1 fm2)

intersectFM' :: Eq key => LeKey key
             -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
intersectFM' le fm1 fm2 = intersectFM_C' le (\ _ right -> right) fm1 fm2

--- Filters only those keys that are bound in both of the given maps
--- and combines the elements as in addToFM_C.
intersectFM_C :: Eq key => (elt -> elt2 -> elt3) -> FM key elt -> FM key elt2
              -> FM key elt3
intersectFM_C combiner (FM le1 fm1) (FM _ fm2) =
  FM le1 (intersectFM_C' le1 combiner fm1 fm2)

intersectFM_C' :: Eq key => LeKey key -> (elt -> elt2 -> elt3)
               -> FiniteMap key elt -> FiniteMap key elt2 -> FiniteMap key elt3
intersectFM_C' _  _        _        EmptyFM = EmptyFM
intersectFM_C' _  _        EmptyFM (BranchFM _ _ _ _ _) = EmptyFM
intersectFM_C' le combiner (BranchFM split_key1 elt1 s1 left1 right1)
                           (BranchFM split_key elt2 _ left right)

  | isJust maybe_elt1   -- split_elt *is* in intersection
  = mkVBalBranch le split_key (combiner elt1' elt2)
                 (intersectFM_C' le combiner lts left)
                 (intersectFM_C' le combiner gts right)

  | otherwise           -- split_elt is *not* in intersection
  = glueVBal le (intersectFM_C' le combiner lts left)
                (intersectFM_C' le combiner gts right)

  where
    fm1 = BranchFM split_key1 elt1 s1 left1 right1
    lts = splitLT le fm1 split_key      -- NB gt and lt, so the equal ones
    gts = splitGT le fm1 split_key      -- are not in either.

    maybe_elt1 = lookupFM' le fm1 split_key
    Just elt1'  = maybe_elt1

-------------------------------------------------------------
--  MAPPING, FOLDING, FILTERING on finite maps
-------------------------------------------------------------

--- Folds finite map by given function.
foldFM :: (key -> elt -> a -> a) -> a -> FM key elt -> a
foldFM k z (FM le fm) = foldFM' le k z fm

foldFM' :: LeKey key -> (key -> elt -> a -> a) -> a -> FiniteMap key elt -> a
foldFM' _  _ z EmptyFM = z
foldFM' le k z (BranchFM key elt _ fm_l fm_r)
  = foldFM' le k (k key elt (foldFM' le k z fm_r)) fm_l

--- Applies a given function on every element in the map.
mapFM :: (key -> elt1 -> elt2) -> FM key elt1 -> FM key elt2
mapFM f (FM le fm) = FM le (mapFM' le f fm)

mapFM' :: LeKey key -> (key -> elt1 -> elt2)
       -> FiniteMap key elt1 -> FiniteMap key elt2
mapFM' _  _ EmptyFM = EmptyFM
mapFM' le f (BranchFM key elt size fm_l fm_r)
  = BranchFM key (f key elt) size (mapFM' le f fm_l) (mapFM' le f fm_r)

--- Yields a new finite map with only those key/element pairs matching the
--- given predicate.
filterFM  :: Eq key => (key -> elt -> Bool) -> FM key elt -> FM key elt
filterFM p (FM le fm) = FM le (filterFM' le p fm)

filterFM' :: Eq key => LeKey key -> (key -> elt -> Bool)
          -> FiniteMap key elt -> FiniteMap key elt
filterFM' _  _ EmptyFM = EmptyFM
filterFM' le p (BranchFM key elt _ fm_l fm_r)
  | p key elt          -- Keep the item
  = mkVBalBranch le key elt (filterFM' le p fm_l) (filterFM' le p fm_r)

  | otherwise          -- Drop the item
  = glueVBal le (filterFM' le p fm_l) (filterFM' le p fm_r)

-----------------------------------------------------
-- INTERROGATING finite maps
-----------------------------------------------------

--- How many elements does given map contain?
sizeFM :: FM _ _ -> Int
sizeFM (FM _ EmptyFM)               = 0
sizeFM (FM _ (BranchFM _ _ size _ _)) = size

sizeFM' :: FiniteMap _ _ -> Int
sizeFM' EmptyFM              = 0
sizeFM' (BranchFM _ _ size _ _) = size

--- Do two given maps contain the same key/element pairs?
eqFM :: (Eq key, Eq elt) => FM key elt -> FM key elt -> Bool
fm_1 `eqFM` fm_2 =
  (sizeFM   fm_1 == sizeFM   fm_2) &&   -- quick test
  (fmToList fm_1 == fmToList fm_2)

--- Is the given finite map empty?
isEmptyFM        :: FM _ _ -> Bool
isEmptyFM fm = sizeFM fm == 0

--- Does given map contain given key?
elemFM :: Eq key => key -> FM key _ -> Bool
key `elemFM` fm = isJust (lookupFM fm key)

--- Retrieves element bound to given key
lookupFM :: Eq key => FM key elt -> key -> Maybe elt
lookupFM (FM le fm) key = lookupFM' le fm key

lookupFM' :: Eq key => LeKey key -> FiniteMap key elt -> key -> Maybe elt
lookupFM' _  EmptyFM _   = Nothing
lookupFM' le (BranchFM key elt _ fm_l fm_r) key_to_find
  = if le key_to_find key
    then lookupFM' le fm_l key_to_find
    else if key_to_find==key
         then Just elt
         else lookupFM' le fm_r key_to_find


--- Retrieves element bound to given key.
--- If the element is not contained in map, return
--- default value.
lookupWithDefaultFM :: Eq key => FM key elt -> elt -> key -> elt
lookupWithDefaultFM fm deflt key
  = case lookupFM fm key of
      Nothing -> deflt
      Just elt -> elt

--- Retrieves the ordering on which the given finite map is built.
keyOrder :: FM key _ -> (key->key->Bool)
keyOrder (FM lt _) = lt

--- Retrieves the smallest key/element pair in the finite map
--- according to the basic key ordering.
minFM :: FM a b -> Maybe (a,b)
minFM = min . tree
  where
   min EmptyFM            = Nothing
   min (BranchFM k x _ l _) | isBranchFM l = min l
                            | otherwise    = Just (k,x)

--- Retrieves the greatest key/element pair in the finite map
--- according to the basic key ordering.
maxFM :: FM a b -> Maybe (a,b)
maxFM = max . tree
  where
    max EmptyFM            = Nothing
    max (BranchFM k x _ _ r) | isBranchFM r = max r
                             | otherwise    = Just (k,x)



----------------------------------------------------
-- LISTIFYING: transform finite maps to lists
----------------------------------------------------

--- Builds a list of key/element pairs. The list is ordered
--- by the initially given irreflexive order predicate on keys.
fmToList        :: FM key elt -> [(key,elt)]
fmToList fm = foldFM (\ key elt rest -> (key,elt) : rest) [] fm

--- Retrieves a list of keys contained in finite map.
--- The list is ordered
--- by the initially given irreflexive order predicate on keys.
keysFM                :: FM key _ -> [key]
keysFM fm   = foldFM (\ key _   rest -> key : rest)       [] fm

--- Retrieves a list of elements contained in finite map.
--- The list is ordered
--- by the initially given irreflexive order predicate on keys.
eltsFM                :: FM _ elt -> [elt]
eltsFM fm   = foldFM (\ _   elt rest -> elt : rest)       [] fm

--- Retrieves list of key/element pairs in preorder of the internal tree.
--- Useful for lists that will be retransformed into a tree or to match
--- any elements regardless of basic order.

fmToListPreOrder :: FM key elt -> [(key,elt)]
fmToListPreOrder (FM _ fm) = pre fm []
   where
     pre EmptyFM xs = xs
     pre (BranchFM k x _ l r) xs = (k,x):pre l (pre r xs)

--- Sorts a given list by inserting and retrieving from finite map.
--- Duplicates are deleted.
fmSortBy :: Eq key => LeKey key -> [key] -> [key]
fmSortBy p l = keysFM (listToFM p (zip l (repeat ())))

-----------------------------------------------------
-- reading/showing finite maps
-----------------------------------------------------

--- Transforms a finite map into a string. For efficiency reasons,
--- the tree structure is shown which is valid for reading only if one
--- uses the same ordering predicate.
showFM :: FM _ _ -> String
showFM (FM _ fm) = showQTerm fm

--- Transforms a string representation of a finite map into a finite map.
--- One has two provide the same ordering predicate as used in the
--- original finite map.
readFM :: LeKey key -> String -> FM key _
readFM p s = FM p (readQTerm s)

-----------------------------------------------------
-- internal Implementation
-----------------------------------------------------

data FM key elt = FM (LeKey key) (FiniteMap key elt)

tree :: FM key elt -> FiniteMap key elt
tree (FM _ fm) = fm

data FiniteMap key elt
  = EmptyFM
  | BranchFM key elt             -- Key and elt stored here
    Int{-STRICT-}              -- Size >= 1
    (FiniteMap key elt)        -- Children
    (FiniteMap key elt)

isEmptyFM' :: FiniteMap _ _ -> Bool
isEmptyFM' fm = sizeFM' fm == 0

isBranchFM :: FiniteMap _ _ -> Bool
isBranchFM (BranchFM _ _ _ _ _) = True
isBranchFM EmptyFM              = False

-------------------------------------------------------------------------
--                                                                      -
--  The implementation of balancing                                     -
--                                                                      -
-------------------------------------------------------------------------
-------------------------------------------------------------------------
--                                                                      -
--  Basic construction of a FiniteMap                                   -
--                                                                      -
-------------------------------------------------------------------------
sIZE_RATIO :: Int
sIZE_RATIO = 5

mkBranch :: Int
         -> key -> elt
         -> FiniteMap key elt -> FiniteMap key elt
         -> FiniteMap key elt

mkBranch _{-which-} key elt fm_l fm_r =
    let result = BranchFM key elt (unbox (1 + left_size + right_size)) fm_l fm_r
    in
      result
      --    if sizeFM result <= 8 then
      --     result
      --    else
      --      pprTrace ("mkBranch:"++(show which)) (ppr result) (
      --      result
      --      )
  where
    {-left_ok  = case fm_l of
                 EmptyFM                         -> True
                 BranchFM _ _ _ _ _  -> cmpWithBiggest_left_key key

    cmpWithBiggest_left_key key' = le (fst (findMax fm_l)) key'

    right_ok = case fm_r of
                 EmptyFM                         -> True
                 BranchFM _ _ _ _ _ -> cmpWithSmallest_right_key key

    cmpWithSmallest_right_key key' = le key' (fst (findMin fm_r))

    balance_ok = True -- sigh-}
    left_size  = sizeFM' fm_l
    right_size = sizeFM' fm_r


    unbox :: Int -> Int
    unbox x = x


-------------------------------------------------------------------------
--                                                                        -
-- Balanced construction of a FiniteMap                                 -
--                                                                        -
-------------------------------------------------------------------------
mkBalBranch :: key -> elt
            -> FiniteMap key elt -> FiniteMap key elt
            -> FiniteMap key elt

mkBalBranch key elt fm_L fm_R

  | size_l + size_r < 2
  = mkBranch 1{-which-} key elt fm_L fm_R

  | size_r > sIZE_RATIO * size_l        -- Right tree too big
  = case fm_R of
        BranchFM _ _ _ fm_rl fm_rr ->
              if sizeFM' fm_rl < 2 * sizeFM' fm_rr
                then single_L fm_L fm_R
                else double_L fm_L fm_R
        -- Other case impossible
        EmptyFM -> error "FiniteMap.mkBalBranch"

  | size_l > sIZE_RATIO * size_r        -- Left tree too big
  = case fm_L of
        BranchFM _ _ _ fm_ll fm_lr ->
              if sizeFM' fm_lr < 2 * sizeFM' fm_ll
                then single_R fm_L fm_R
                else double_R fm_L fm_R
        -- Other case impossible
        EmptyFM -> error "FiniteMap.mkBalBranch"

  | otherwise                                -- No imbalance
  = mkBranch 2{-which-} key elt fm_L fm_R

  where
    size_l   = sizeFM' fm_L
    size_r   = sizeFM' fm_R

    single_L fm_l (BranchFM key_r elt_r _ fm_rl fm_rr)
        = mkBranch 3{-which-} key_r elt_r (mkBranch 4{-which-} key elt fm_l fm_rl) fm_rr
    single_L _ EmptyFM = error "FiniteMap.single_L"

    double_L fm_l (BranchFM key_r elt_r _ (BranchFM key_rl elt_rl _ fm_rll fm_rlr) fm_rr)
        = mkBranch 5{-which-} key_rl elt_rl (mkBranch 6{-which-} key   elt   fm_l   fm_rll)
                                 (mkBranch 7{-which-} key_r elt_r fm_rlr fm_rr)
    double_L _ EmptyFM = error "FiniteMap.double_L"
    double_L _ (BranchFM _ _ _ EmptyFM _) = error "FiniteMap.double_L"

    single_R (BranchFM key_l elt_l _ fm_ll fm_lr) fm_r
        = mkBranch 8{-which-} key_l elt_l fm_ll (mkBranch 9{-which-} key elt fm_lr fm_r)
    single_R EmptyFM _ = error "FiniteMap.single_R"

    double_R (BranchFM key_l elt_l _ fm_ll (BranchFM key_lr elt_lr _ fm_lrl fm_lrr)) fm_r
        = mkBranch 10{-which-} key_lr elt_lr (mkBranch 11{-which-} key_l elt_l fm_ll  fm_lrl)
                                 (mkBranch 12{-which-} key   elt   fm_lrr fm_r)
    double_R EmptyFM _ = error "FiniteMap.double_R"
    double_R (BranchFM _ _ _ _ EmptyFM) _ = error "FiniteMap.double_R"


mkVBalBranch :: Eq key => (LeKey key)
             -> key -> elt
             -> FiniteMap key elt -> FiniteMap key elt
             -> FiniteMap key elt

-- Assert: in any call to (mkVBalBranch_C comb key elt l r),
--           (a) all keys in l are < all keys in r
--           (b) all keys in l are < key
--           (c) all keys in r are > key

mkVBalBranch le key elt EmptyFM fm_r = addToFM' le fm_r key elt
mkVBalBranch le key elt (BranchFM key_l elt_l s_l fm_ll fm_lr) EmptyFM =
   addToFM' le (BranchFM key_l elt_l s_l fm_ll fm_lr) key elt

mkVBalBranch le key elt (BranchFM key_l elt_l s_l fm_ll fm_lr)
                        (BranchFM key_r elt_r s_r fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (mkVBalBranch le key elt fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (mkVBalBranch le key elt fm_lr fm_r)

  | otherwise
  = mkBranch 13{-which-} key elt fm_l fm_r

  where
    fm_l = BranchFM key_l elt_l s_l fm_ll fm_lr
    fm_r = BranchFM key_r elt_r s_r fm_rl fm_rr
    size_l = sizeFM' fm_l
    size_r = sizeFM' fm_r

-------------------------------------------------------------------------
--                                                                        -
-- Gluing two trees together                                            -
--                                                                        -
-------------------------------------------------------------------------
glueBal :: (LeKey key)
        -> FiniteMap key elt -> FiniteMap key elt
        -> FiniteMap key elt

glueBal le fm1 fm2 =
  if isEmptyFM' fm1
    then fm2
    else if isEmptyFM' fm2
           then fm1
           else
        -- The case analysis here (absent in Adams' program) is really to deal
        -- with the case where fm2 is a singleton. Then deleting the minimum means
        -- we pass an empty tree to mkBalBranch, which breaks its invariant.
             let (mid_key1, mid_elt1) = findMax fm1
                 (mid_key2, mid_elt2) = findMin fm2
             in
             if sizeFM' fm2 > sizeFM' fm1
               then mkBalBranch mid_key2 mid_elt2 fm1 (deleteMin le fm2)
               else mkBalBranch mid_key1 mid_elt1 (deleteMax le fm1) fm2

glueVBal :: (LeKey key)
         -> FiniteMap key elt -> FiniteMap key elt
         -> FiniteMap key elt

glueVBal le fm_l fm_r =
  if isEmptyFM' fm_l
    then fm_r
    else if isEmptyFM' fm_r
           then fm_l
           else
             let BranchFM key_l elt_l _ fm_ll fm_lr = fm_l
                 BranchFM key_r elt_r _ fm_rl fm_rr = fm_r
                 --(mid_key_l,mid_elt_l) = findMax fm_l
                 --(mid_key_r,mid_elt_r) = findMin fm_r
                 size_l = sizeFM' fm_l
                 size_r = sizeFM' fm_r
             in
               if sIZE_RATIO * size_l < size_r
               then
                 mkBalBranch key_r elt_r (glueVBal le fm_l fm_rl) fm_rr
                else if sIZE_RATIO * size_r < size_l
                    then
                      mkBalBranch key_l elt_l fm_ll (glueVBal le fm_lr fm_r)

                      -- We now need the same two cases as in glueBal above.
                    else glueBal le fm_l fm_r

-------------------------------------------------------------------------
--                                                                        -
-- Local utilities                                                      -
--                                                                        -
-------------------------------------------------------------------------

splitLT, splitGT :: Eq key => (LeKey key) -> FiniteMap key elt -> key
                    -> FiniteMap key elt

-- splitLT fm split_key  =  fm restricted to keys <  split_key
-- splitGT fm split_key  =  fm restricted to keys >  split_key

splitLT _  EmptyFM _ = EmptyFM
splitLT le (BranchFM key elt _ fm_l fm_r) split_key
  = if le split_key key
    then splitLT le fm_l split_key
    else if split_key == key
         then fm_l
         else mkVBalBranch le key elt fm_l (splitLT le fm_r split_key)

splitGT _  EmptyFM _ = EmptyFM
splitGT le (BranchFM key elt _ fm_l fm_r) split_key
  = if le split_key key
    then mkVBalBranch le key elt (splitGT le fm_l split_key) fm_r
    else if split_key == key
         then fm_r
         else splitGT le fm_r split_key

findMin :: FiniteMap key elt -> (key,elt)
findMin EmptyFM = error "FiniteMap.findMin: empty map"
findMin (BranchFM key elt _ EmptyFM _) = (key,elt)
findMin (BranchFM _   _   _ (BranchFM key_l elt_l s_l fm_ll fm_lr)_) =
      findMin (BranchFM key_l elt_l s_l fm_ll fm_lr)

deleteMin :: (LeKey key) -> FiniteMap key elt -> FiniteMap key elt
deleteMin _  EmptyFM                           = error "FiniteMap.deleteMin: empty map"
deleteMin _  (BranchFM _   _   _ EmptyFM fm_r) = fm_r
deleteMin le (BranchFM key elt _ (BranchFM key_l elt_l s_l fm_ll fm_lr) fm_r) =
  mkBalBranch key elt (deleteMin le (BranchFM key_l elt_l s_l fm_ll fm_lr))
                         fm_r

findMax :: FiniteMap key elt -> (key,elt)
findMax EmptyFM = error "FiniteMap.findMax: empty map"
findMax (BranchFM key elt _ _ EmptyFM) = (key,elt)
findMax (BranchFM _   _   _ _  (BranchFM key_r elt_r s_r fm_rl fm_rr)) =
  findMax (BranchFM key_r elt_r s_r fm_rl fm_rr)

deleteMax :: (LeKey key) -> FiniteMap key elt -> FiniteMap key elt
deleteMax _  EmptyFM                           = error "FiniteMap.deleteMax: empty map"
deleteMax _  (BranchFM _   _   _ fm_l EmptyFM) = fm_l
deleteMax le (BranchFM key elt _ fm_l (BranchFM key_r elt_r s_r fm_rl fm_rr)) =
  mkBalBranch key elt fm_l
              (deleteMax le (BranchFM key_r elt_r s_r fm_rl fm_rr))



-------------------------------------------------------------------------
--                                                                      -
--   FiniteSets---a thin veneer                                         -
--                                                                      -
-------------------------------------------------------------------------
type FiniteSet key = FM key ()
emptySet         :: (LeKey key) -> FiniteSet key
mkSet            :: Eq key => (LeKey key) -> [key] -> FiniteSet key
isEmptySet       :: FiniteSet _ -> Bool
elementOf        :: Eq key => key -> FiniteSet key -> Bool
minusSet         :: Eq key => FiniteSet key -> FiniteSet key -> FiniteSet key
setToList        :: FiniteSet key -> [key]
union            :: Eq key => FiniteSet key -> FiniteSet key -> FiniteSet key

emptySet = emptyFM
mkSet le xs = listToFM le [ (x, ()) | x <- xs]
isEmptySet = isEmptyFM
elementOf = elemFM
minusSet  = minusFM
setToList = keysFM
union = plusFM


