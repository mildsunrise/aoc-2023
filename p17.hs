{-# LANGUAGE TupleSections, TypeApplications, ScopedTypeVariables #-}

import Data.Foldable (forM_)
import Control.Arrow (Arrow(first))
import Control.Monad (when, guard)
import Control.Monad.ST.Strict (ST, runST)
import Data.Array (array, bounds, (!), Ix (inRange))
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef)
import qualified Data.Set as Set
import Data.Maybe (fromJust)

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

dijkstra
  (roots :: [(d, node)])
  (isEnd :: node -> Bool)
  (neighbors :: node -> [(d, node)])
  (dequeue :: m (d, node))
  (alterDistance :: node -> (Maybe d -> Maybe d) -> m ())
  = forM_ roots addRoot >> run
  where
  addRoot (d, node) = alterDistance node (\_ -> Just d)
  run = dequeue >>= step
  step (d, node)
    | isEnd node = return d
    | otherwise  = process d node >> run
  process d node = forM_ (neighbors node) $ \(c, neigh) ->
    alterDistance neigh (newDistance (d + c))
  newDistance d' = (d' <$) . guard . maybe True (d' <)

runArrayDijkstra bounds roots isEnd neighbors heuristic = runST $ do
  distances <- newSTArray bounds Nothing
  queue <- newSTRef Set.empty
  let
    dequeue = do
      (_, node) <- alterSTRef queue Set.deleteFindMin
      (, node) . fromJust <$> readArray distances node
    alterDistance node f = do
      d <- readArray distances node
      forM_ (f d) $ \d' -> do
        writeArray distances node (Just d')
        let h = heuristic node
        let f = maybe id (Set.delete . (, node) . (+ h)) d
        modifySTRef queue (Set.insert (d' + h, node) . f)
  dijkstra roots isEnd neighbors dequeue alterDistance

pzip f (a, b) (c, d) = (f a c, f b d)
pmap f (a, b) = (f a, f b)


-- PARTS

minHeatLoss (da, db) costs =
  runArrayDijkstra nodeBounds roots isEnd neighbors heuristic
  where
  (bs, be) = bounds costs
  nodeBounds = ((bs, (0, 0)), (be, (3, db)))
  roots = withCosts bs $ map (,0) [0..3]
  isEnd = (== be) . fst
  -- TODO: use da

  headings = [(1, 0), (0, 1), (-1, 0), (0, -1)]
  newPos (h, a) = pzip (+) $ headings !! h
  heuristic = uncurry (+) . pzip (-) be . fst

  withCosts pos =
    map (\node -> (costs ! fst node, node)) .
    filter (inRange nodeBounds) .
    map (\ha -> (newPos ha pos, ha))

  neighbors (pos, (h, a)) =
    withCosts pos $
    ((h, a+1) :) $
    map ((,0) . (`mod` 4) . (h +)) [-1, 1]

part k =
  minHeatLoss k .
  mapToArray .
  map (map (read @Int . (: [])))

(part1, part2) = (part (1, 2), part (4, 6))
