{-# LANGUAGE TupleSections #-}

import Data.Array (array, bounds, (!), Ix (inRange))
import Data.List (elemIndex, delete)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines

pzip f (a, b) (c, d) = (f a c, f b d)

preprocessGraph (neighbors, (start, end)) =
  ((visit Map.empty start Map.!), (start, end))
  where
  visit graph node
    | Map.member node graph = graph
    | otherwise = foldl visit graph' (map fst neighs)
    where
    neighs = map (followPath node 1) (neighbors node)
    graph' = Map.insert node neighs graph

  followPath parent l node =
    case delete parent (neighbors node) of
      [next] -> followPath node (l + 1) next
      _      -> (node, l)


-- PARTS

buildGraph slippery tiles = (neighbors, ends)
  where
  ends = pzip (pzip (+)) ((1,0), (-1,0)) (bounds tiles)

  neighbors node =
    filter ((/= '#') . (tiles !)) $
    filter (inRange $ bounds tiles) $
    map (pzip (+) node) $
    nodeHeadings (tiles ! node)

  nodeHeadings c
    | c == '.' || not slippery = headings
    | otherwise = [ fromJust $ lookup c trails ]

  headings = [(1, 0), (0, 1), (-1, 0), (0, -1)]
  trails = zip ">v<^" headings

possiblePaths (graph, (start, end)) = dfs Map.empty (start, 0)
  where
  dfs path (node, l)
    | node == end          = [ sum $ Map.elems path' ]
    | Map.member node path = []
    | otherwise            = graph node >>= dfs path'
    where
    path' = Map.insert node l path

part slippery =
  maximum .
  possiblePaths .
  preprocessGraph .
  buildGraph slippery .
  mapToArray

(part1, part2) = (part True, part False)
