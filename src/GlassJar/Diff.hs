-- |
-- Module      : GlassJar.Diff
-- Description : Line-level diff generation.
-- Copyright   : (c) Flechazo, 2026
-- License     : MIT
-- Maintainer  : 2558755403@qq.com
--
-- Defines behavior for converting two UTF-8 text contents into minimal
-- line-level edits and context lines.
module GlassJar.Diff
  ( lineDiff,
  )
where

import Data.Array (Array, listArray, (!))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import GlassJar.Types (DiffLine (..))

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Returns minimal line-level differences between two text contents.
--
-- @ctx@ limits preserved context lines around each changed block.
lineDiff :: Int -> BSL.ByteString -> BSL.ByteString -> [DiffLine]
lineDiff ctx old new =
  addContext ctx $ myersDiff (toLines old) (toLines new)

-- Splits a lazy ByteString into Text lines.
toLines :: BSL.ByteString -> [T.Text]
toLines = go
  where
    go bs
      | BSL.null bs = []
      | otherwise =
          case BSL.elemIndex 10 bs of -- 10 = '\n'
            Nothing -> [decodeLine bs]
            Just i ->
              let (l, rest) = BSL.splitAt i bs
               in decodeLine l : go (BSL.drop 1 rest)
    decodeLine = TE.decodeUtf8With lenientDecode . BSL.toStrict

-------------------------------------------------------------------------------
-- Context trimming
-------------------------------------------------------------------------------

-- Limits context runs to at most @ctx@ lines on each side.
addContext :: Int -> [DiffLine] -> [DiffLine]
addContext _ [] = []
addContext ctx ds =
  let (eqs, rest) = spanL isKeep ds
      (chgs, rest2) = spanL (not . isKeep) rest
      (eqs2, rest3) = spanL isKeep rest2
   in trimCtx ctx eqs ++ chgs ++ trimCtx ctx eqs2 ++ addContext ctx rest3
  where
    isKeep (DiffKeep _) = True
    isKeep _ = False

    trimCtx _ [] = []
    trimCtx c xs
      | length xs <= 2 * c = xs
      | otherwise = take c xs ++ drop (length xs - c) xs

-- Strict left-span.
spanL :: (a -> Bool) -> [a] -> ([a], [a])
spanL p = go []
  where
    go acc [] = (reverse acc, [])
    go acc (x : xs)
      | p x = go (x : acc) xs
      | otherwise = (reverse acc, x : xs)

-------------------------------------------------------------------------------
-- Myers diff (trace + backtrack)
-------------------------------------------------------------------------------

-- Returns a minimal edit script for two line sequences.
myersDiff :: [T.Text] -> [T.Text] -> [DiffLine]
myersDiff old new =
  let n = length old
      m = length new
      oldArr = listArray (0, n - 1) old
      newArr = listArray (0, m - 1) new
      trace = shortestEditTrace oldArr newArr n m
   in backtrackDiff oldArr newArr n m trace

-- Computes a sequence of edit frontiers, one per edit depth.
shortestEditTrace ::
  Array Int T.Text ->
  Array Int T.Text ->
  Int ->
  Int ->
  [IM.IntMap Int]
shortestEditTrace oldArr newArr n m =
  go 0 (IM.singleton 1 0) []
  where
    maxD = n + m

    go d v trace
      | d > maxD = trace
      | reached = trace'
      | otherwise = go (d + 1) vNext trace'
      where
        trace' = trace ++ [v]
        (vNext, reached) = expandLayer d v

    expandLayer d vPrev = loopK [-d, (-d + 2) .. d] IM.empty False
      where
        loopK [] vCur doneFlag = (vCur, doneFlag)
        loopK _ vCur True = (vCur, True)
        loopK (k : ks) vCur _ =
          let leftX = IM.findWithDefault 0 (k - 1) vPrev
              downX = IM.findWithDefault 0 (k + 1) vPrev
              xStart =
                if k == (-d) || (k /= d && leftX < downX)
                  then downX
                  else leftX + 1
              yStart = xStart - k
              (xEnd, yEnd) = followSnake oldArr newArr n m xStart yStart
              vCur' = IM.insert k xEnd vCur
              done' = xEnd >= n && yEnd >= m
           in loopK ks vCur' done'

-- Walks backward through edit frontiers to produce a minimal diff.
backtrackDiff ::
  Array Int T.Text ->
  Array Int T.Text ->
  Int ->
  Int ->
  [IM.IntMap Int] ->
  [DiffLine]
backtrackDiff oldArr newArr n m trace = go (length trace - 1) n m []
  where
    go d x y acc
      | d < 0 = acc
      | otherwise =
          let v = trace !! d
              k = x - y
              leftX = IM.findWithDefault 0 (k - 1) v
              downX = IM.findWithDefault 0 (k + 1) v
              prevK =
                if k == (-d) || (k /= d && leftX < downX)
                  then k + 1
                  else k - 1
              prevX = IM.findWithDefault 0 prevK v
              prevY = prevX - prevK
              (accAfterSnake, x1, _y1) = consumeSnake acc x y prevX prevY
              accAfterEdit
                | d <= 0 = accAfterSnake
                | x1 == prevX = DiffAdd (newArr ! prevY) : accAfterSnake
                | otherwise = DiffDel (oldArr ! prevX) : accAfterSnake
           in go (d - 1) prevX prevY accAfterEdit

    consumeSnake acc x y prevX prevY
      | x > prevX && y > prevY =
          consumeSnake (DiffKeep (oldArr ! (x - 1)) : acc) (x - 1) (y - 1) prevX prevY
      | otherwise = (acc, x, y)

-- Skips consecutive matching lines from a given position.
followSnake ::
  Array Int T.Text ->
  Array Int T.Text ->
  Int ->
  Int ->
  Int ->
  Int ->
  (Int, Int)
followSnake oldArr newArr n m x y
  | x >= 0 && y >= 0 && x < n && y < m && oldArr ! x == newArr ! y =
      followSnake oldArr newArr n m (x + 1) (y + 1)
  | otherwise = (x, y)
