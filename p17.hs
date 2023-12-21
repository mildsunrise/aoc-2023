{-# LANGUAGE TupleSections, TypeApplications, ScopedTypeVariables #-}

import Data.Foldable (forM_)
import Control.Arrow (Arrow(first))
import Control.Monad (when, guard)
import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (MaybeT))
import Data.Array (array, bounds, (!), Ix (inRange), Array)
import Data.Array.ST (STArray, newArray, readArray, writeArray, freeze)
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

freezeSTArray :: (Ix i) => STArray s i e -> ST s (Array i e)
freezeSTArray = freeze

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

tryDeleteMin s
  | Set.null s = (Nothing, s)
  | otherwise  = first Just $ Set.deleteFindMin s

runArrayDijkstra bounds roots isEnd neighbors heuristic = runST $ do
  distances <- newSTArray bounds Nothing
  queue <- newSTRef Set.empty
  let
    dequeue = do
      (d, node) <- MaybeT $ alterSTRef queue tryDeleteMin
      lift $ (, node) . fromJust <$> readArray distances node
    alterDistance node f = lift $ do
      d <- readArray distances node
      forM_ (f d) $ \d' -> do
        writeArray distances node (Just d')
        let h = heuristic node
        let f = maybe id (Set.delete . (, node) . (+ h)) d
        modifySTRef queue (Set.insert (d' + h, node) . f)
  d <- runMaybeT $ dijkstra roots isEnd neighbors dequeue alterDistance
  (d,) <$> freezeSTArray distances

pzip f (a, b) (c, d) = (f a c, f b d)
pmap f (a, b) = (f a, f b)


-- PARTS

minHeatLoss (da, db) costs =
  fst $ runArrayDijkstra nodeBounds roots isEnd neighbors heuristic
  where
  firstPass = fmap fromJust $ snd $ runArrayDijkstra
    (bs, be) [(0, be)] (const False) preNeighbors (const 0)
  preNeighbors node =
    map (\node -> (costs ! node, node)) $
    filter (inRange (bs, be)) $
    map (pzip (+) node) headings

  (bs, be) = bounds costs
  nodeBounds = ((bs, (0, 0)), (be, (3, db)))
  roots = withCosts bs $ map (,0) [0..3]
  isEnd = (== be) . fst
  -- TODO: use da

  headings = [(1, 0), (0, 1), (-1, 0), (0, -1)]
  newPos (h, a) = pzip (+) $ headings !! h
  heuristic = (firstPass !) . fst

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
