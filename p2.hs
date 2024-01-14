{- HLINT ignore "Use map once" -}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications #-}

import Data.List.Split (splitOn)
import qualified Data.Map as Map

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseGame (splitOn ":" ->
  [ words -> ["Game", read @Int -> id]
  , map parseSet . splitOn ";" -> sets
  ]) = (id, sets)

parseSet = Map.fromList . map parsePart . splitOn ","

parsePart (words -> [read @Int -> n, color]) = (color, n)


-- CONSTANTS

loaded = Map.fromList
  [ ("red", 12)
  , ("green", 13)
  , ("blue", 14) ]


-- PART 1

isSetPossible =
  Map.null .
  Map.filter (< 0) .
  Map.unionWith (+) loaded .
  fmap negate

isGamePossible =
  all isSetPossible .
  snd

part1 =
  sum .
  map fst .
  filter isGamePossible .
  map parseGame


-- PART 2

gamePower =
  product .
  map snd .
  Map.toList .
  Map.unionsWith max .
  snd

part2 =
  sum .
  map gamePower .
  map parseGame
