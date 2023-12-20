{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
import Data.List (intercalate, transpose, sort, group, find)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import qualified Data.Map as Map
import Data.Foldable (foldl', forM_, foldlM)
import Data.Array.Unboxed (UArray, array, bounds, (!), Array, IArray)
import Data.Array.ST (thaw, runSTUArray, readArray, writeArray, getBounds, runSTArray, Ix, STArray, STUArray, MArray)
import Control.Monad.ST.Strict (ST)
import Data.Array.Base (thawSTUArray)
import Data.Tuple (swap)
import Data.Bits (Bits(setBit))
import Data.Word (Word8)
import Control.Monad (replicateM_)
import Control.Arrow (Arrow(second, first))
import Data.Bifunctor (Bifunctor(bimap))

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

unsnoc xs = (init xs, last xs)

splitParts (unsnoc -> (parts, '}')) = splitOn "," parts

parseInput (splitOn [""] ->
  [ Map.fromList . map parseWorkflow -> workflows
  , map parsePart -> parts
  ]) = (workflows, parts)

parseWorkflow (splitOn "{" ->
  [ name, unsnoc . splitParts ->
    ( map parseRule -> rules, parseDest -> fallback )
  ] ) = (name, (rules, fallback))

parseRule (splitOn ":" ->
  [ parseCondition -> condition, parseDest -> dest ]
  ) = (condition, dest)

parseCondition (c : op : (read @Int -> x)) = (c, op, x)

parseDest "A"  = Left True
parseDest "R"  = Left False
parseDest name = Right name

parsePart ('{' : (splitParts -> ratings)) =
  Map.fromList $ map parseRating ratings

parseRating (k : '=' : (read @Int -> v)) = (k, v)


-- PART 1

runWorkflows workflows part = run "in"
  where
  run name =
    either id run $
    runWorkflow $
    workflows Map.! name

  runWorkflow (rules, fallback) =
    maybe fallback snd $
    find (runCondition . fst) rules

  runCondition (c, op, x) =
    runOp op (part Map.! c) x

  runOp '<' = (<)
  runOp '>' = (>)

part1 =
  sum .
  map (sum . Map.elems) .
  uncurry (filter . runWorkflows) .
  parseInput


-- PART 2

base = Map.fromList $ map (, (1, 4000)) "xmas"

rangeCombinations (a, b) = max 0 $ b - a + 1

partCombinations =
  product .
  map (toInteger . rangeCombinations) .
  Map.elems

part2 =
  sum .
  map partCombinations .
  evalWorkflows base .
  fst .
  parseInput
