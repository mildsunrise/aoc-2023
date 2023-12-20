{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}

import Data.List (intercalate, transpose, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import qualified Data.Map as Map

main = getContents >>= (print . part2 . lines)

parts x = (part1 x, part2 x)


-- PART 1

rot90 = transpose . reverse
applyN n f x = iterate f x !! n

lineGravity =
  intercalate "#" .
  map sort .
  splitOn "#"

tiltR = map lineGravity
tiltU = applyN 3 rot90 . tiltR . rot90

totalLoad =
  sum .
  zipWith (*) [1..] .
  map (length . filter (== 'O')) .
  reverse

part1 = totalLoad . tiltU


-- PART 2

findCycle = (`helper` Map.empty) . zip [0..]
  where
  helper ((i,x):xs) m =
    either (m,) (helper xs) $
    Map.alterF (maybe (Right $ Just i) Left) x m

cycleIndex i (m, start) =
  min i $
  (+ start) $
  (`mod` (Map.size m - start)) $
  (+ (-start)) i

queryCycle i cycle =
  fromJust $
  lookup (cycleIndex i cycle) $
  map swap $
  Map.toList (fst cycle)

smartApplyN i f = queryCycle i . findCycle . iterate f

tiltCycle = applyN 4 (tiltR . rot90)

part2 = totalLoad . smartApplyN 1000000000 tiltCycle
