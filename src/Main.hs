module Main where

import System.Environment (getArgs)

import Perl.Monad

import My.Loader

main = do
  args <- getArgs
  let
    perlFilename = fetchPerlFilename args
  runPerlT $ loadHasperl perlFilename

fetchPerlFilename = go where
  go [] = "/dev/stdin"
  go (('-':_):[]) = "/dev/stdin"
  go (('-':_):a:as) = go as
  go (a:_) = a