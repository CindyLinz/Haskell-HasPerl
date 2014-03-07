{-# LANGUAGE FlexibleInstances, UndecidableInstances, RankNTypes #-}
module My.MonadUtil
  () where

import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Catch
import qualified Control.Monad.IO.Class as MTL
import Exception (ExceptionMonad (..))
import GhcMonad (liftGhcT)
import MonadUtils (MonadIO (..))
import GHC (GhcT)

import Perl.Type
import Perl.Monad

instance ExceptionMonad (PerlT s IO) where
  --gmask = (mask :: (((PerlT s IO) a -> (PerlT s IO) a) -> (PerlT s IO) b) -> (PerlT s IO) b)
  gcatch = catch

--instance ExceptionMonad m => ExceptionMonad (PerlT s m) where
--  gmask = perlMask gmask
--  gcatch = perlCatch gcatch

instance (MonadCatch m, MTL.MonadIO m) => MonadIO (PerlT s m) where
  liftIO = lift . MTL.liftIO

instance MonadTrans GhcT where
  lift = liftGhcT

