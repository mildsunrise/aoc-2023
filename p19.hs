{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications, TupleSections #-}

import Data.List.Split (splitOn)
import Control.Monad (guard, (<=<))
import Control.Monad.Extra (findM)
import Control.Monad.State (StateT(StateT), execStateT)
import Control.Arrow (Arrow(second, first))
import qualified Data.Map as Map

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

runOp '<' = (<)
runOp '>' = (>)

runWorkflows runCondition workflows = run "in"
  where
  run name =
    either return run <=<
    runWorkflow $
    workflows Map.! name

  runWorkflow (rules, fallback) =
    maybe fallback snd <$>
    findM (runCondition . fst) rules

part1 =
  sum .
  map (sum . Map.elems) .
  uncurry (filter . runWorkflows impl) .
  parseInput

  where
  impl (c, op, x) part =
    runOp op (part Map.! c) x


-- PART 2

base = Map.fromList $ map (, (1, 4000)) "xmas"

runOpRange op (a, b) x =
  filter (uncurry (<=) . snd) $
  zip [True, False] (f op)
  where
  f '<' = [(a, min (x-1) b), (max x a, b)]
  f '>' = [(max (x+1) a, b), (a, min x b)]

partCombinations =
  product .
  map (\(a, b) -> b - a + 1) .
  Map.elems

part2 =
  sum .
  map partCombinations .
  (`execStateT` base) .
  (guard <=< runWorkflows impl) .
  fst .
  parseInput

  where
  impl (c, op, x) = StateT $ \part ->
    map (second $ \r -> Map.insert c r part) $
    runOpRange op (part Map.! c) x
