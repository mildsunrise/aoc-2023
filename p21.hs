{-# LANGUAGE TupleSections, FlexibleContexts #-}

import Control.Monad (when, filterM)
import Control.Monad.ST.Lazy (ST, runST)
import Data.Array (array, bounds, (!), Ix (inRange), assocs)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.List.Split (chunksOf)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

newSTArray :: (Ix i) => (i, i) -> e -> ST s (STArray s i e)
newSTArray = newArray

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines

pzip f (a, b) (c, d) = (f a c, f b d)
pmap f (a, b) = (f a, f b)

iterateM f x = (x :) <$> (f x >>= iterateM f)

runArrayBFS bounds roots neighbors = runST $ do
  visited <- newSTArray bounds False
  let
    round = filterM visit . concatMap neighbors
    visit node = do
      new <- not <$> readArray visited node
      when new $ writeArray visited node True
      return new
  mapM_ visit roots
  iterateM round roots


-- PART 1

headings = [(1, 0), (0, 1), (-1, 0), (0, -1)]

start =
  fst .
  head .
  filter ((== 'S') . snd) .
  assocs

reachableGardens tiles =
  runArrayBFS (bounds tiles) [start tiles] neighbors
  where
  neighbors node =
    filter ((/= '#') . (tiles !)) $
    filter (inRange $ bounds tiles) $
    map (pzip (+) node) headings

part1 =
  sum .
  map head .
  chunksOf 2 .
  take 65 .
  map length .
  reachableGardens .
  mapToArray


-- PART 2

part2 = const "TODO"
