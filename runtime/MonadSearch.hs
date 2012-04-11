module MonadSearch where

import ID
import Control.Monad
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict

class MonadPlus m => MonadSearch m where
  szero :: Cover -> FailInfo -> m a
  splus :: Cover -> ID -> m a -> m a -> m a
  ssum  :: Cover -> ID -> [m a] -> m a
  constrainMSearch :: Cover -> Constraints -> m a -> m a

  szero _ _ = mzero
  splus _ _ = mplus
  ssum  _ _ = msum

instance MonadSearch m => MonadSearch (Lazy.StateT a m) where
  splus cd i mx my = Lazy.StateT (\s -> splus cd i (Lazy.runStateT mx s) (Lazy.runStateT my s))
  ssum  cd i mxs   = Lazy.StateT (\s -> ssum cd i (map (flip Lazy.runStateT s) mxs))
  szero cd info = Lazy.StateT (\_ -> szero cd info)
  constrainMSearch cd cs mx = Lazy.StateT (\s -> constrainMSearch cd cs (Lazy.runStateT mx s))

instance MonadSearch m => MonadSearch (Strict.StateT a m) where
  splus cd i mx my = Strict.StateT (\s -> splus cd i (Strict.runStateT mx s) (Strict.runStateT my s))
  ssum  cd i mxs   = Strict.StateT (\s -> ssum cd i (map (flip Strict.runStateT s) mxs))
  szero cd info = Strict.StateT (\_ -> szero cd info)
  constrainMSearch cd cs mx = Strict.StateT (\s -> constrainMSearch cd cs (Strict.runStateT mx s))
