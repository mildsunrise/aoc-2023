{-# LANGUAGE TypeApplications, TupleSections #-}

import Data.Bifunctor (Bifunctor(bimap))
import Data.List (groupBy, nub)
import Data.Function (on)
import Data.Array (array, Ix (inRange), bounds, (!), assocs, listArray, (//))
import Data.Char (isDigit)
import Data.Maybe (catMaybes)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PART 1

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines

runsBy pred =
  filter (pred . head) .
  groupBy (on (==) pred)

getLineNumbers y =
  map (bimap (map (,y)) (read @Int) . unzip) .
  runsBy (isDigit . snd) .
  zip [0..]

getNumbers = concat . zipWith getLineNumbers [0..]

adjacentCells arr (x, y) =
  map (arr !) $
  filter (inRange $ bounds arr) $
  concatMap (\x -> map (x,) [y-1..y+1]) [x-1..x+1]

isSymbol c = not (c == '.' || isDigit c)

isPartNumber arr = any (any isSymbol . adjacentCells arr) . fst

part1 lines =
  sum $
  map snd $
  filter (isPartNumber $ mapToArray lines) $
  getNumbers lines


-- PART 2

cellNumbers lines =
  (blankArr //) $
  concatMap (\(cells,n) -> map (, Just n) cells) $
  getNumbers lines
  where
    blankArr = listArray (bounds $ mapToArray lines) (repeat Nothing)

adjacentNumbers lines =
  nub .
  catMaybes .
  adjacentCells (cellNumbers lines) .
  fst

part2 lines =
  sum $
  map product $
  filter ((== 2) . length) $
  map (adjacentNumbers lines) $
  filter ((== '*') . snd) $
  assocs $
  mapToArray lines
