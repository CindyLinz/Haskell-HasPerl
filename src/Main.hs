module Main where

import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class

import System.Environment (getArgs)

import Perl.Monad

import My.Loader

main :: IO ()
main = do
  args <- getArgs
  let
    perlFilename = fetchPerlFilename args
  runPerlT $ do
    prepareLoader
    Catch.catch (loadHasperl perlFilename) (\e -> liftIO $ putStr $ show (e :: Catch.SomeException))

fetchPerlFilename = go where
  go [] = "/dev/stdin"
  go (('-':_):[]) = "/dev/stdin"
  go (('-':_):a:as) = go as
  go (a:_) = a
