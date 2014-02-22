module My.MonadUtil
  () where

import Control.Monad.Trans.Class (MonadTrans (..))
import Exception (ExceptionMonad (..))
import GhcMonad (liftGhcT)
import MonadUtils (MonadIO (..))
import GHC (GhcT)

import Perl.Monad
import Perl.Monad.Exception

instance ExceptionMonad m => ExceptionMonad (PerlT s m) where
  gmask = perlMask gmask
  gcatch = perlCatch gcatch

instance MonadIO m => MonadIO (PerlT s m) where
  liftIO = lift . liftIO

instance MonadTrans GhcT where
  lift = liftGhcT

