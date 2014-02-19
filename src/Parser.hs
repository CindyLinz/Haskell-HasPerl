module Parser where

import Data.Char

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BB

data Lexeme
  = PlPart BL.ByteString
  | HsExpr BL.ByteString
  | HsDecl BL.ByteString
  deriving Show

parse
  :: BL.ByteString -- ^ hasperl
  -> (BL.ByteString, BL.ByteString) -- ^ (perl, haskell)
parse hspl = undefined

extractHaskell
  :: BL.ByteString -- ^ hasperl
  -> [Lexeme]
extractHaskell = takePerl where
  takePerl :: BL.ByteString -> [Lexeme]
  takePerl perl = finalRes where
    (former, later') = BL.break (== fromIntegral (ord '#')) perl
    (middle, later) = BL.break (== fromIntegral (ord '{')) later'
    middleLen = BL.length middle
    (selfRes, laterRes) = if BL.null later
      then (former `BL.append` middle, [])
      else if middleLen == 1
      then -- into haskell expr
        (former, takeHs HsExpr (BL.tail later))
      else if middleLen == 2 && BL.last middle == fromIntegral (ord '#')
        then -- into haskell decl
          (former, takeHs HsDecl (BL.tail later))
        else -- still in perl
          case takePerl later of
            PlPart some : others -> (former `BL.append` middle `BL.append` some, others)
            allOthers -> (former `BL.append` middle, allOthers)
    finalRes = if BL.null selfRes
      then laterRes
      else PlPart selfRes : laterRes
  takeHs :: (BL.ByteString -> Lexeme) -> BL.ByteString -> [Lexeme]
  takeHs con = go BL.empty 1 where
    go acc 0 hs = con (BL.init acc) : takePerl hs
    go acc depth hs = go acc' depth' hs' where
      (former, later) = BL.break (\c -> c == fromIntegral (ord '{') || c == fromIntegral (ord '}')) hs
      hs' = BL.tail later
      laterHead = BL.head later
      acc' = acc `BL.append` former `BL.snoc` laterHead
      depth' = if laterHead == fromIntegral (ord '{')
        then depth+1
        else depth-1
