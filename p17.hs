{-# LANGUAGE TupleSections, TypeApplications #-}

import Data.Foldable (forM_)
import Control.Arrow (Arrow(first))
import Control.Monad (when)
import Control.Monad.ST.Strict (ST, runST)
import Data.Array (array, bounds, (!), Ix (inRange))
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef)
import qualified Data.Set as Set

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

alterSTRef ref f = do
  (a, x') <- f <$> readSTRef ref
  a <$ writeSTRef ref x'

newSTArray :: (Ix i) => (i, i) -> e -> ST s (STArray s i e)
newSTArray = newArray

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines

dijkstra isEnd neighbors dequeue getDist setDist = run
  where
  run = dequeue >>= step
  step (d, node)
    | isEnd node = return d
    | otherwise  = process d node >> run
  process d node =
    forM_ (neighbors node) (adjust . first (d +))
  adjust (d', node) = do
    d <- getDist node
    when (maybe True (d' <) d) $ setDist node d d'

pzip f (a, b) (c, d) = (f a c, f b d)
pmap f (a, b) = (f a, f b)


-- PARTS

minHeatLoss (da, db) costs = runST $ do
  let
    (bs, be) = bounds costs
    nodeBounds = ((bs, (0, 0)), (be, (3, db)))
    roots = withCosts bs $ map (,0) [0..3]
    isEnd = (== be) . fst
    -- TODO: use da

    headings = [(1, 0), (0, 1), (-1, 0), (0, -1)]
    newPos (h, a) = pzip (+) $ headings !! h

    withCosts pos =
      map (\node -> (costs ! fst node, node)) .
      filter (inRange nodeBounds) .
      map (\ha -> (newPos ha pos, ha))

    neighbors (pos, (h, a)) =
      withCosts pos $
      ((h, a+1) :) $
      map ((,0) . (`mod` 4) . (h +)) [-1, 1]

  dists <- newSTArray nodeBounds Nothing
  queue <- newSTRef $ Set.fromList roots
  let
    dequeue = alterSTRef queue Set.deleteFindMin
    getDist = readArray dists
    setDist node d d' = do
      writeArray dists node (Just d')
      let f = maybe id (Set.delete . (, node)) d
      modifySTRef queue (f . Set.insert (d', node))
  dijkstra isEnd neighbors dequeue getDist setDist

part k =
  minHeatLoss k .
  mapToArray .
  map (map (read @Int . (: [])))

(part1, part2) = (part (1, 2), part (4, 6))
