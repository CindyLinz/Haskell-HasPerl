module My.Loader
  ( loadHasperl
  , prepareLoader
  ) where

import Control.Applicative
import qualified Control.Monad.Catch as Catch
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
import Perl.Accessor hiding (eval)

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
    dirs <- cap "@INC" >>= readArray
    let
      filename = strReplace modName "::" "/" ++ ".hspm"
      tryLoad [] = die $ "Can't locate " ++ filename ++ " in @INC (@INC contains: " ++ unwords dirs ++ ") at HasPerl"
      tryLoad (dir:dirs) = Catch.handle (\e -> let _e = e :: Catch.SomeException in tryLoad dirs) (loadHasperl (dir ++ "/" ++ filename))
    loaded <- readScalar =<< cap "%INC" %- filename 
    if loaded == (0 :: Int)
      then do
        ret <- tryLoad dirs
        if ret == (0 :: Int)
          then die $ filename ++ " did not return a true value at HasPerl"
          else do
            cap "%INC" %- filename $= (1 :: Int)
            return ret
      else return (1 :: Int)

loadHasperl :: Retrievable ret => FilePath -> PerlT s IO ret
loadHasperl path = do
  (perlPath, haskellPath) <- liftIO $ My.Parser.compileFile path
  res <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
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
      lift $ Catch.try $ eval perl
  case res of
    Right result -> return result
    Left e -> Catch.throwM (e :: Catch.SomeException)
