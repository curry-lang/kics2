-- ---------------------------------------------------------------------------
-- | IDSupply implementation using GHC's UniqSupply
-- ---------------------------------------------------------------------------
module IDSupply
  ( IDSupply, initSupply, leftSupply, rightSupply, unique
  , Unique, mkInteger, showUnique
  , getDecisionRaw, setDecisionRaw, unsetDecisionRaw
  ) where

import Control.Monad    (liftM)
import Data.IORef       (IORef, newIORef, readIORef, modifyIORef)
import Data.Maybe       (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import UniqDFM          (UniqDFM, emptyUDFM, delFromUDFM, lookupUDFM, addToUDFM)
import UniqSupply       (UniqSupply, mkSplitUniqSupply, splitUniqSupply,
                         uniqFromSupply)
import Unique           (Unique, getKey)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Decision, defaultDecision, isDefaultDecision)

-- |References to 'Decision's are represented using GHC's 'UniqSupply'
newtype IDSupply = IDSupply { uniqSupply :: UniqSupply }

instance Eq IDSupply where
  s1 == s2 = unique s1 == unique s2

instance Show IDSupply where
  show = showUnique . unique

-- |Retrieve an 'Unique' representation of the unique identifier
mkInteger :: Unique -> Integer
mkInteger = toInteger . getKey

showUnique :: Unique -> String
showUnique = tail . show -- tail to avoid showing of leading 'a'

-- |Initialize a new 'IDSupply'
initSupply :: IO IDSupply
initSupply = IDSupply `liftM` mkSplitUniqSupply 'a'

leftSupply :: IDSupply -> IDSupply
leftSupply = IDSupply . fst . splitUniqSupply . uniqSupply

rightSupply :: IDSupply -> IDSupply
rightSupply = IDSupply . snd . splitUniqSupply . uniqSupply

unique :: IDSupply -> Unique
unique = uniqFromSupply . uniqSupply

-- |Internal store for 'Decision's
store :: IORef (UniqDFM Decision)
store = unsafePerformIO (newIORef emptyUDFM)
{-# NOINLINE store #-}

getDecisionRaw :: Unique -> IO Decision
getDecisionRaw u = (fromMaybe defaultDecision . flip lookupUDFM u)
                   `liftM` readIORef store

setDecisionRaw :: Unique -> Decision -> IO ()
setDecisionRaw u c
  | isDefaultDecision c = modifyIORef store $ flip delFromUDFM u -- collect garbage
  | otherwise           = modifyIORef store $ \ m -> addToUDFM m u c

unsetDecisionRaw :: Unique -> IO ()
unsetDecisionRaw = modifyIORef store . flip delFromUDFM
