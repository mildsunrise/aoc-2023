{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections, ViewPatterns, FlexibleContexts #-}

import Control.Applicative ((<|>))
import Data.List.Split (splitOn)
import Data.List (unfoldr)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main = getContents >>= (print . part . lines)


-- GENERIC ALGORITHMS

dup a = (a, a)
pairs a = zip a (tail a)

spanningTree graph root = dfs Nothing root Map.empty
  where
  dfs parent node =
    (\(v, m) -> maybe (recurse node m) (const m) v) .
    Map.alterF (\v -> (v, v <|> Just parent)) node
  recurse node m =
    foldl (flip (dfs (Just node))) m (graph Map.! node)

tracePath parents node =
  node : unfoldr (fmap dup . (parents Map.!)) node

deleteEdge (a, b) = deleteDir a b . deleteDir b a
  where
  deleteDir a b = Map.update (Just . Set.delete b) a

minCut start end = run 0
  where
  run i graph
    | Map.member end parents = run (i + 1) graph'
    | otherwise = (i, Map.size parents)
    where
    parents = spanningTree graph start
    edges = pairs $ tracePath parents end
    graph' = foldl (flip deleteEdge) graph edges


-- PARSING

parseLine (
  splitOn ": " -> [name, words -> dests]
  ) = (name, dests)


-- PART

buildGraph xs =
  fmap Set.fromList $
  Map.fromListWith (++) $
  (++ xs) $
  concatMap (\(i, ys) -> map (, [i]) ys) xs

cutSize graph =
  (\(Just n) -> n * (Map.size graph - n)) $
  lookup 3 $
  (\(n:ns) -> map (\m -> minCut n m graph) ns) $
  Map.keys graph

part =
  cutSize .
  buildGraph .
  map parseLine
