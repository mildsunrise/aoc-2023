{- HLINT ignore "Use list literal" -}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications, TupleSections #-}

import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Bifunctor (Bifunctor(bimap))
import Control.Applicative (Alternative((<|>)))

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseAlmanac (splitOn [""] ->
  [parseSeeds -> seeds] : (map parseMap -> maps)
  ) = (seeds, maps)

parseSeeds (splitOn ":" ->
  ["seeds", map (read @Int) . words -> seeds]
  ) = seeds

parseMap (_ : (map parseRange -> ranges)) = ranges

parseRange (map (read @Int) . words -> [b,a,n]) = (a,(b,n))


-- PARTS

lookupRange i (a, (b, n))
  | i - a < n = Just (i - a + b, Just (a + n - i))
  | otherwise = Nothing

lookupMapStep i m =
  fromMaybe (i, (+ (-i)) . fst <$> Map.lookupGT i m) $
  (>>= lookupRange i) $
  Map.lookupLE i m

lookupMap m (i, n) = helper $ lookupMapStep i m
  where
  helper (y, Just k) | k < n = (y, k) : lookupMap m (i + k, n - k)
  helper (y, _)              = (y, n) : []

lookupMaps =
  foldl (flip (.)) id .
  map (concatMap . lookupMap . Map.fromList)

part f =
  minimum .
  map fst .
  uncurry (flip ($)) .
  bimap f lookupMaps .
  parseAlmanac

part1 = part (map (,1))
part2 = part (map (\[a,b] -> (a,b)) . chunksOf 2)
