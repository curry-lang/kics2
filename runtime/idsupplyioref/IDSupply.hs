-- ---------------------------------------------------------------------------
-- | IDSupply implementation using IORefs and GHC's UniqSupply
-- ---------------------------------------------------------------------------
module IDSupply
  ( IDSupply, initSupply, leftSupply, rightSupply, unique
  , Unique, mkInteger, showUnique
  , Store (..)
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO  (unsafeDupableInterleaveIO)
import UniqSupply
  (UniqSupply, mkSplitUniqSupply, splitUniqSupply, uniqFromSupply)
import qualified Unique as GHC (Unique, getKey)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Decision, defaultDecision)

data Unique = Unique { unqRef:: (IORef Decision), unqKey :: GHC.Unique }

instance Eq Unique where
  Unique ref1 _ == Unique ref2 _ = ref1 == ref2

instance Show Unique where
  show = showUnique . unqKey

data IDSupply = IDSupply
  { unique      :: Unique   -- ^ Decision and unique identifier for this IDSupply
  , leftSupply  :: IDSupply -- ^ path to the left IDSupply
  , rightSupply :: IDSupply -- ^ path to the right IDSupply
  }

instance Eq IDSupply where
  s1 == s2 = unique s1 == unique s2

instance Show IDSupply where
  show = show . unique -- tail to avoid showing of leading 'a'

-- |Retrieve an 'Integer' representation of the unique identifier
mkInteger :: Unique -> Integer
mkInteger = toInteger . GHC.getKey . unqKey

showUnique :: Unique -> String
showUnique = tail . show -- tail to avoid showing of leading 'a'

-- |Initialize a new 'IDSupply'
initSupply :: IO IDSupply
initSupply = mkSplitUniqSupply 'a' >>= getPureSupply

-- |Internal construction of an 'IDSupply' using 'unsafeDupableInterleaveIO'
-- to enable a lazy construction of the child 'IDSupply's inside the 'IO'
-- monad.
-- Without this unsafe function, the construction would loop infinitely.
--
-- /Note:/ Previously, this was implemented using 'unsafePerformIO', but
-- as 'unsafePerformIO' traverse the entire call stack to perform blackholing
-- this resulted in a very bad performance.
--
-- For more information, see
-- <http://www.haskell.org/pipermail/glasgow-haskell-users/2011-March/020223.html>
getPureSupply :: UniqSupply -> IO IDSupply
getPureSupply uniqS = do
  let (leftS, rightS) = splitUniqSupply uniqS
  s1 <- unsafeDupableInterleaveIO $ getPureSupply leftS
  s2 <- unsafeDupableInterleaveIO $ getPureSupply rightS
  r  <- unsafeDupableInterleaveIO $ newIORef defaultDecision
  return (IDSupply (Unique r (uniqFromSupply uniqS)) s1 s2)
{-# NOINLINE getPureSupply #-}

-- |Type class for a Decision 'Store'
class (Monad m) => Store m where
  -- |Get the stored 'Decision', defaulting to 'defaultDecision'
  getDecisionRaw    :: Unique -> m Decision
  -- |Set the 'Decision'
  setDecisionRaw    :: Unique -> Decision -> m ()
  -- |Unset the 'Decision'
  unsetDecisionRaw  :: Unique -> m ()

instance Store IO where
  getDecisionRaw    u   = readIORef  (unqRef u)
  setDecisionRaw    u c = writeIORef (unqRef u) c
  unsetDecisionRaw  u   = writeIORef (unqRef u) defaultDecision
