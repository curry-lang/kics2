------------------------------------------------------------------------------
--- An implementation of double-ended queues supporting access at both
--- ends in constant amortized time.
---
--- @author Bernd Brassel, Olaf Chitil, Michael Hanus, Sebastian Fischer,
---         Bjoern Peemoeller
--- @version December 2018
------------------------------------------------------------------------------

module Data.Queue
  ( -- Abstract data type, constructors and queries
    Queue, empty, cons, snoc, isEmpty, deqLength
    -- Selectors
  , deqHead, deqTail, deqLast, deqInit, deqReverse, rotate, matchHead, matchLast
    -- conversion from and to lists
  , listToDeq, deqToList
  ) where

--- The datatype of a queue.
data Queue a = S Int [a] Int [a]

--- The empty queue.
empty :: Queue _
empty = S 0 [] 0 []

--- Inserts an element at the front of the queue.
cons :: a -> Queue a -> Queue a
cons x (S lenf f lenr r) = check (lenf + 1) (x : f) lenr r

--- Inserts an element at the end of the queue.
snoc :: a -> Queue a -> Queue a
snoc x (S lenf f lenr r) = deqReverse (check (lenr + 1) (x : r) lenf f)

--- Is the queue empty?
isEmpty :: Queue _ -> Bool
isEmpty (S lenf _ lenr _) = lenf + lenr == 0

--- Returns the number of elements in the queue.
deqLength :: Queue _ -> Int
deqLength (S lenf _ lenr _) = lenf + lenr

--- The first element of the queue.
deqHead :: Queue a -> a
deqHead (S lenf f _ r) = head (if lenf == 0 then r else f)

--- Removes an element at the front of the queue.
deqTail :: Queue a -> Queue a
deqTail (S _    []     _    _) = empty
deqTail (S lenf (_:fs) lenr r) = deqReverse (check lenr r (lenf - 1) fs)

--- The last element of the queue.
deqLast :: Queue a -> a
deqLast (S _ f lenr r) = head (if lenr == 0 then f else r)

--- Removes an element at the end of the queue.
deqInit :: Queue a -> Queue a
deqInit (S _    _ _    []    ) = empty
deqInit (S lenf f lenr (_:rs)) = check lenf f (lenr - 1) rs

--- Reverses a double ended queue.
deqReverse :: Queue a -> Queue a
deqReverse (S lenf f lenr r) = S lenr r lenf f

--- Moves the first element to the end of the queue.
rotate :: Queue a -> Queue a
rotate q = snoc (deqHead q) (deqTail q)

--- Matches the front of a queue.
--- `matchHead q` is equivalent to
--- `if isEmpty q then Nothing else Just (deqHead q, deqTail q)`
--- but more efficient.
matchHead :: Queue a -> Maybe (a, Queue a)
matchHead (S _    []     _    []     ) = Nothing
matchHead (S _    []     _    [x]    ) = Just (x, empty)
matchHead (S _    []     _    (_:_:_))
  = error $ "Data.Queue.matchHead: illegal queue"
matchHead   (S lenf (x:xs) lenr r      )
  = Just (x, deqReverse (check lenr r (lenf - 1) xs))

--- Matches the end of a queue.
--- `matchLast q` is equivalent to
--- `if isEmpty q then Nothing else Just (deqLast q,deqInit q)`
--- but more efficient.
matchLast :: Queue a -> Maybe (a,Queue a)
matchLast (S _    []      _    []    ) = Nothing
matchLast (S _    [x]     _    []    ) = Just (x, empty)
matchLast (S _    (_:_:_) _    []    )
  = error $ "Data.Queue.matchLast: illegal queue"
matchLast   (S lenf f       lenr (x:xs)) = Just (x, check lenf f (lenr - 1) xs)

--- Transforms a list to a double ended queue.
listToDeq :: [a] -> Queue a
listToDeq xs = check (length xs) xs 0 []

--- Transforms a double ended queue to a list.
deqToList :: Queue a -> [a]
deqToList (S _ xs _ ys) = xs ++ reverse ys

--- Check for invariant: The length of the first list is smaller than
--- three times the length of the second plus 1.
check :: Int -> [a] -> Int -> [a] -> Queue a
check lenf f lenr r
  | lenf <= 3 * lenr + 1 = S lenf f lenr r
  | otherwise            = S lenf' f' lenr' r'
  where
    len       = lenf + lenr
    lenf'     = len `div` 2
    lenr'     = len - lenf'
    (f', rf') = splitAt lenf' f
    r'        = r ++ reverse rf'
