{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections, FlexibleContexts #-}

import Data.Foldable (forM_)
import Control.Monad (unless)
import Control.Monad.ST (ST, runST)
import Data.Array (array, bounds, (!), Ix (inRange))
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.STRef (newSTRef, readSTRef, modifySTRef')

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

onlyOnce arr x m = do
  marked <- readArray arr x
  unless marked (writeArray arr x True >> m)


-- CONSTANTS

headings = [(1, 0), (0, 1), (-1, 0), (0, -1)]

outHeadings '.'  h = [h]
outHeadings '/'  h = [3 - h]
outHeadings '\\' h = [(1 - h) `mod` 4]
outHeadings '-'  h | odd h = [0, 2]
outHeadings '-'  h = [h]
outHeadings '|'  h | even h = [1, 3]
outHeadings '|'  h = [h]

rayPositions (xs, ys) (xe, ye) =
  concat $
  zipWith (\h -> map (,h)) [0..]
  [ map (xs,) [ys..ye], map (,ys) [xs..xe] ,
    map (xe,) [ys..ye], map (,ye) [xs..xe] ]


-- PARTS

part input = map dfs roots
  where
  tiles = mapToArray input
  (bs, be) = bounds tiles
  nodeBounds = ((bs, 0), (be, 3))
  roots = rayPositions bs be
  newPos pos h = pzip (+) pos $ headings !! h

  neighbors (pos, h) =
    filter (inRange (bs, be) . fst) $
    map (\h -> (newPos pos h, h)) $
    outHeadings (tiles ! pos) h

  dfs root = runST $ do
    visited <- newSTArray nodeBounds False
    energized <- newSTArray (bs, be) False
    count <- newSTRef 0
    let
      visit node = onlyOnce visited node $ do
        onlyOnce energized (fst node) $
          modifySTRef' count (+ 1)
        forM_ (neighbors node) visit
    visit root
    readSTRef count

(part1, part2) = (head . part, maximum . part)
