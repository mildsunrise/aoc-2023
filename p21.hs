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

iterateM f x = (x :) <$> (f x >>= iterateM f)

markArray arr k = do
  new <- not <$> readArray arr k
  when new $ writeArray arr k True
  return new

runArrayBFS bounds roots neighbors = runST $ do
  visited <- newSTArray bounds False
  let
    round = filterM visit . concatMap neighbors
    visit = markArray visited
  mapM_ visit roots
  iterateM round roots


-- PART 1

headings = [(1, 0), (0, 1), (-1, 0), (0, -1)]

findStart =
  fst .
  head .
  filter ((== 'S') . snd) .
  assocs

reachableGardens tiles start =
  concat $
  scanl1 (zipWith (+)) $
  chunksOf 2 $
  map length $
  runArrayBFS (bounds tiles) [start] neighbors
  where
  neighbors node =
    filter ((/= '#') . (tiles !)) $
    filter (inRange $ bounds tiles) $
    map (pzip (+) node) headings

part1 =
  (!! 64) .
  (\t -> reachableGardens t $ findStart t) .
  mapToArray


-- PART 2

bigReachableGardens dist tiles = let
  e = fst $ snd $ bounds tiles
  s = e + 1
  hs = s `div` 2
  k = (dist - 3) `div` s - 2

  axis =
    map (count (hs + 1))
    [(0,hs), (e,hs), (hs,0), (hs,e)]

  diagonals =
    map (zipWith (*) [k+1..] . count (s + 1))
    [(0,0), (0,e), (e,0), (e,e)]

  count startDist =
    reverse .
    take (dist' `div` s + 1) .
    map head .
    chunksOf s .
    drop (dist' `mod` s) .
    reachableGardens tiles
    where
    dist' = dist - s * k - startDist

  (pe : po : _) =
    drop (2 * s + dist `mod` 2) $
    reachableGardens tiles (0, 0)

  (hk, ek) = (k `div` 2, k - hk)
  fullCopies = pe * (hk + ek*ek) + po * (k + hk*hk)
  partialCopies = sum $ concat $ axis ++ diagonals
  in pe + 4 * fullCopies + partialCopies

part2 =
  bigReachableGardens 26501365 .
  mapToArray
