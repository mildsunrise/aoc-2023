{-# LANGUAGE ViewPatterns, TupleSections #-}

import Data.Maybe (mapMaybe)
import Data.Array (array, inRange, bounds, (!), assocs)
import Control.Monad (guard)
import qualified Data.Map as Map

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- CONSTANTS

tileToJunctions = (Map.!) $
  Map.fromList [
  ('|', Just (1,3)),
  ('-', Just (0,2)),

  ('L', Just (0,1)),
  ('J', Just (1,2)),
  ('7', Just (2,3)),
  ('F', Just (3,0)),

  ('.', Nothing),
  ('S', Nothing)]

followJunctions (a, b) i
  | i == a = Just b
  | i == b = Just a
  | otherwise = Nothing

cellOffset = (!!) [
  ( 1, 0),
  ( 0,-1),
  (-1, 0),
  ( 0, 1)]


-- PART 1

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines

iterateMaybe f x = x : maybe [] (iterateMaybe f) (f x)

pzip f (a,b) (c,d) = (f a c, f b d)

part (mapToArray -> net) = head $ mapMaybe followLoop [0..3]
  where
  start = fst $ head $ filter ((== 'S') . snd) $ assocs net

  followOutJunction (pos, o) = do
    let ipos = pzip (+) pos $ cellOffset o
    guard $ inRange (bounds net) ipos
    return (ipos, (o + 2) `mod` 4)

  followJunction (pos, i) = do
    junctions <- tileToJunctions (net ! pos)
    o <- followJunctions junctions i
    followOutJunction (pos, o)

  followLoop o = do
    first <- followOutJunction (start, o)
    let steps = iterateMaybe followJunction first
    guard (fst (last steps) == start) >> return steps

part1 = (`div` 2) . length . part


-- PART 2

pointArea (p, h) = fst p * snd (cellOffset h)

polygonArea = abs . sum . map pointArea

interiorTiles x = polygonArea x - length x `div` 2 + 1

part2 = interiorTiles . part
