module My.Loader
  ( loadHasperl
  , prepareLoader
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Unsafe.Coerce

import Data.List

import DynFlags
import GHC
import GHC.Paths (libdir)
import GhcMonad (liftGhcT)
import MonadUtils (liftIO, MonadIO)

import Perl.Monad
import Perl.Eval
import Perl.Type
import Perl.Sub
import Perl.Constant

import My.MonadUtil
import qualified My.Parser

strReplace :: String -> String -> String -> String
strReplace str find replace = go str where
  findLen = length find
  go str = if find `isPrefixOf` str
    then replace ++ go (drop findLen str)
    else str

prepareLoader :: PerlT s IO ()
prepareLoader = do
  defSub "HasPerl::require" $ \modName -> do
    liftIO $ putStrLn $ "HasPerl::require(" ++ modName ++ ")"
    let
      filename = strReplace modName "::" "/" ++ ".hspm"
    res <- loadHasperl filename
    case res of
      Right sv -> retSub (sv :: SV)
      Left msg -> retSub ()

loadHasperl :: Retrievable ret => FilePath -> PerlT s IO (Either String ret)
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
