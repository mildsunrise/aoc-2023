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


-- PARTS

buildGraph tiles = (neighbors, ends)
  where
  ends = pzip (pzip (+)) ((1,0), (-1,0)) (bounds tiles)

  neighbors node =
    filter ((/= '#') . (tiles !)) $
    filter (inRange $ bounds tiles) $
    map (pzip (+) node) $
    nodeHeadings (tiles ! node)

  nodeHeadings c
    | c == '.'  = headings
    | otherwise = [ fromJust $ lookup c trails ]

  headings = [(1, 0), (0, 1), (-1, 0), (0, -1)]
  trails = zip ">v<^" headings

possiblePaths (graph, (start, end)) = dfs Map.empty start
  where
  dfs path node
    | node == end          = [ Map.size path ]
    | Map.member node path = []
    | otherwise            = graph node >>= dfs path'
    where
    path' = Map.insert node () path

part1 =
  maximum .
  possiblePaths .
  buildGraph .
  mapToArray

part2 = const "TODO"
