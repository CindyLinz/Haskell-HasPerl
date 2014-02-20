{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Char
import Data.String

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BB

data Lexeme
  = PlPart BL.ByteString
  | HsExpr BL.ByteString
  | HsDecl BL.ByteString
  deriving Show

lambdaPrefix :: IsString a => a
lambdaPrefix = "HasPerl::lambda"

lambdaName :: Int -> BL.ByteString
lambdaName ser = BL.pack $ map (fromIntegral . ord) (lambdaPrefix ++ show ser)

analyzePos
  :: Int -- ^ init line num
  -> Int -- ^ current offset
  -> BL.ByteString -- ^ target
  -> (Int, Int) -- ^ (end line num, end offset)
analyzePos lineNo offset target = case BL.uncons target of
  Nothing
    -> (lineNo, offset)
  Just (hd, tl) -> case chr $ fromIntegral hd of
    '\n' -> analyzePos (id $! lineNo + 1) 0 tl
    '\t' -> analyzePos lineNo (id $! offset - offset `mod` 8 + 8) tl
    _ -> analyzePos lineNo (id $! offset + 1) tl

makeSpace
  :: Int
  -> BL.ByteString
makeSpace n = BL.pack $ map (fromIntegral . ord) $ replicate n ' '

makeExpr
  :: Int -- ^ offset
  -> Int -- ^ serial
  -> BL.ByteString -- ^ code
  -> BL.ByteString
makeExpr offset ser code =
  "-- Expr\n" `BL.append`
  lambdaName ser

parse
  :: Int -- ^ serial
  -> BL.ByteString -- ^ hasperl
  -> (BL.ByteString, BL.ByteString) -- ^ (perl, haskell)
parse ser hspl = (perl, haskell) where
  lexemes = extractHaskell hspl

  perl = genPerl ser lexemes
  genPerl ser [] = BL.empty
  genPerl ser (PlPart p : others) = p `BL.append` genPerl ser others
  genPerl ser (HsDecl _ : others) = genPerl ser others
  genPerl ser (HsExpr _ : others) = 32 `BL.cons` lambdaName ser `BL.append` "()" `BL.append` genPerl (id $! ser+1) others

  haskell = haskellDecl `BL.append` initDecl
  initDecl = "init :: PerlT s IO ()\ninit = do { return () " `BL.append` registerSubs `BL.append` "\n}\n" where
    registerSubs = BL.concat $ zipWith decorateCode [ser..] haskellExprs
    decorateCode ser code = "\n; (defSub \"" `BL.append` lambdaName ser `BL.append` "\" :: SubReturn ret => PerlSub s ret -> Perl s ()) $ \n" `BL.append` code

  haskellDecl = genHaskellDecl 0 0 lexemes
  genHaskellDecl lineNo offset lexemes = case lexemes of
    [] -> BL.empty
    PlPart p : others -> genHaskellDecl lineNo' offset' others
      where (lineNo', offset') = analyzePos lineNo offset p
    HsDecl h : others -> "-- Decl\n" `BL.append` makeSpace offset `BL.append` h `BL.append` "\n" `BL.append` genHaskellDecl lineNo' offset' others
      where (lineNo', offset') = analyzePos lineNo offset h
    HsExpr h : others -> genHaskellDecl lineNo' offset' others
      where (lineNo', offset') = analyzePos lineNo offset h

  haskellExprs = genHaskellExprs 0 0 lexemes
  genHaskellExprs lineNo offset lexemes = case lexemes of
    [] -> []
    PlPart p : others -> genHaskellExprs lineNo' offset' others
      where (lineNo', offset') = analyzePos lineNo offset p
    HsDecl h : others -> genHaskellExprs lineNo' offset' others
      where (lineNo', offset') = analyzePos lineNo offset h
    HsExpr h : others -> (makeSpace offset `BL.append` h) : genHaskellExprs lineNo' offset' others
      where (lineNo', offset') = analyzePos lineNo offset h

  ser' = genSer ser lexemes
  genSer ser [] = ser
  genSer ser (PlPart _ : others) = genSer ser others
  genSer ser (HsDecl _ : others) = genSer ser others
  genSer ser (HsExpr _ : others) = genSer (id $! ser+1) others

extractHaskell
  :: BL.ByteString -- ^ hasperl
  -> [Lexeme]
extractHaskell = takePerl where
  takePerl :: BL.ByteString -> [Lexeme]
  takePerl perl = finalRes where
    (former, later') = BL.break (== fromIntegral (ord '#')) perl
    (middle, later) = BL.span (== fromIntegral (ord '#')) later'
    laterHd = BL.head later
    middleLen = BL.length middle
    (selfRes, laterRes) = if BL.null later
      then (perl, [])
      else if middleLen == 1 && laterHd == fromIntegral (ord '{')
      then -- into haskell expr
        (former, takeHs HsExpr (BL.tail later))
      else if middleLen == 2 && laterHd == fromIntegral (ord '{')
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
