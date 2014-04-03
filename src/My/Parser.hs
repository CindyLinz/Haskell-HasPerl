{-# LANGUAGE OverloadedStrings #-}
module My.Parser 
  ( parse
  , compileFile
  ) where

import Data.Char
import Data.String
import Data.List

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BB

stringToByteString = BL.pack . map (fromIntegral . ord)

-- | Generate a/b/c/x.hs and a/b/c/x.pl from a/b/c/x.hspl
compileFile
  :: FilePath -- ^ hspl path
  -> IO (FilePath, FilePath) -- ^ (perl path, haskell path)
compileFile path = do
  src <- BL.readFile path
  let
    (pathPrefix, perlPostfix) =
      if ".hspl" `isSuffixOf` path || ".hspm" `isSuffixOf` path
        then (take (length path - 5) path, '.' : drop (length path - 2) path)
        else (path, ".pl")
    perlPath = pathPrefix ++ perlPostfix
    haskellPath = pathPrefix ++ ".hs"
    namePrefix = "HasPerl::lambda::" ++ toName pathPrefix
    toName [] = []
    toName (c:cs) =
      let others = toName cs
      in case c of
        '.' -> '_' : others
        '/' -> "::" ++ others
        '\\' -> "::" ++ others
        _ -> c : others
    (perl, haskell) = parse path (stringToByteString namePrefix) src
  BL.writeFile perlPath perl
  BL.writeFile haskellPath haskell
  return (perlPath, haskellPath)

data Lexeme
  = PlPart BL.ByteString
  | HsExpr BL.ByteString
  | HsDecl BL.ByteString
  deriving Show

lambdaPrefix :: IsString a => a
lambdaPrefix = "HasPerl::lambda"

lambdaName :: BL.ByteString -> Int -> BL.ByteString
lambdaName prefix ser = prefix `BL.append` stringToByteString (show ser)

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

parse
  :: String -- ^ filename
  -> BL.ByteString -- ^ prefix
  -> BL.ByteString -- ^ hasperl
  -> (BL.ByteString, BL.ByteString) -- ^ (perl, haskell)
parse filename prefix hspl = (perl, haskell) where
  lexemes = extractHaskell hspl

  commentedHsInPerl :: BL.ByteString -> BL.ByteString
  commentedHsInPerl hs =
    let
      (line, remain) = BL.span (/= fromIntegral (ord '\n')) hs
    in if BL.null remain
        then
          ""
        else
          "# " `BL.append` line `BL.append` "\n" `BL.append` commentedHsInPerl (BL.tail remain)

  perl = genPerl 0 lexemes
  genPerl ser [] = BL.empty
  genPerl ser (PlPart p : others) = p `BL.append` genPerl ser others
  genPerl ser (HsDecl h : others) = commentedHsInPerl h `BL.append` genPerl ser others
  genPerl ser (HsExpr h : others) = commentedHsInPerl h `BL.append` (32 `BL.cons` lambdaName prefix ser `BL.append` "()" `BL.append` genPerl (id $! ser+1) others)

  haskell = preludeDecl `BL.append` haskellDecl `BL.append` initDecl
  initDecl = "init :: Perl.Type.PerlT s IO ()\ninit = do { return () " `BL.append` registerSubs `BL.append` "\n}\n" where
    registerSubs = BL.concat $ zipWith decorateCode [0..] haskellExprs
    decorateCode ser code = "\n; (Perl.Sub.defSub \"" `BL.append` lambdaName prefix ser `BL.append` "\" :: Perl.SVArray.ToSVArray ret => Perl.Type.Perl s ret -> Perl.Type.Perl s ()) $ \n" `BL.append` code

  preludeDecl = "import qualified Perl.Type\nimport qualified Perl.Sub\nimport qualified Perl.SVArray\n------\n"

  haskellDecl = genHaskellDecl 0 0 lexemes
  genHaskellDecl lineNo offset lexemes = case lexemes of
    [] -> BL.empty
    PlPart p : others -> genHaskellDecl lineNo' offset' others
      where (lineNo', offset') = analyzePos lineNo offset p
    HsDecl h : others ->
      "{-# LINE "
      `BL.append` stringToByteString (show (lineNo + 1))
      `BL.append` " "
      `BL.append` stringToByteString (show filename)
      `BL.append` " #-} -- Decl\n"
      `BL.append` makeSpace offset
      `BL.append` h
      `BL.append` "\n"
      `BL.append` genHaskellDecl lineNo' offset' others
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
    HsExpr h : others ->
      ( "{-# LINE "
      `BL.append` stringToByteString (show (lineNo + 1))
      `BL.append` " "
      `BL.append` stringToByteString (show filename)
      `BL.append` " #-}\n"
      `BL.append` makeSpace (offset + 2)
      `BL.append` h
      ) : genHaskellExprs lineNo' offset' others
      where (lineNo', offset') = analyzePos lineNo offset h

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
