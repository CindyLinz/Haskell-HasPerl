module My.Loader
  ( loadHasperl
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Unsafe.Coerce

import DynFlags
import GHC
import GHC.Paths (libdir)
import GhcMonad (liftIO, liftGhcT)
import MonadUtils (MonadIO)

import Perl.Monad
import Perl.Eval

import My.MonadUtil
import qualified My.Parser

loadHasperl :: FilePath -> PerlT s IO ()
loadHasperl path = do
  (perlPath, haskellPath) <- liftIO $ My.Parser.compileFile path
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhcT (Just libdir) $ do
      dflags <- getSessionDynFlags
      let pkg = PkgConfFile "/home/cindy/.ghc/x86_64-linux-7.6.3/package.conf.d/"
      setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                  , ghcLink   = LinkInMemory
                                  --, packageFlags = ExposePackage "time" : packageFlags dflags
                                  , extraPkgConfs = (pkg:) . extraPkgConfs dflags
                                  --, verbosity = 0
                                  }
      setTargets =<< sequence [guessTarget haskellPath Nothing]
      load LoadAllTargets
      setContext [IIModule $ mkModuleName "Main"]

      modInit <- unsafeCoerce <$> compileExpr "Main.init"
      lift modInit

      perl <- liftIO $ readFile perlPath
      lift $ eval perl

      return ()
