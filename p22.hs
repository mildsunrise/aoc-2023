{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections, TypeApplications, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (forM, forM_, when)
import Control.Monad.ST.Strict (ST, runST)
import Data.Array (bounds, (!), Ix (inRange, range), accumArray, elems)
import Data.Array.ST (STArray, newArray, readArray, writeArray, freeze)
import Data.List.Split (splitOn)
import Data.List (sortOn)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Containers.ListUtils (nubOrd)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

newSTArray :: (Ix i) => (i, i) -> e -> ST s (STArray s i e)
newSTArray = newArray


-- PARSING

parseVec (
  map (read @Int) . splitOn "," -> [x,y,z]
  ) = ((x,y), z)

parseLine (
  map parseVec . splitOn "~" -> [a, b]
  ) = (a, b)


-- PARTS

buildGraph xs = runST $ do
  let
    stat f g = bimap f f $ unzip $ map (fst . g) xs
    bs = (stat minimum fst, stat maximum snd)
    preprocessPiece ((xy1, z1), (xy2, z2)) =
      (range (xy1, xy2), z2 - z1 + 1)
    pieces = zip [1..] $ map preprocessPiece xs

  arr <- newSTArray bs (0, 0)
  forM pieces $ \(i, (xys, h)) -> do
    under <- forM xys (readArray arr)
    let z = maximum $ map fst under
    forM_ xys $ \xy -> writeArray arr xy (z + h, i)
    let prev = filter ((== z) . fst) under
    pure $ (i,) $ nubOrd $ map snd prev

reverseGraph xs =
  accumArray (flip (:)) [] (0, length xs) $
  concatMap (\(i,ys) -> map (,i) ys) xs

disintegrateBrick graph i = runST $ do
  visited <- newSTArray (bounds graph) False
  let
    dfs node = do
      seen <- readArray visited node
      writeArray visited node True
      when (not seen && node /= i) $
        mapM_ dfs (graph ! node)
  dfs 0
  length . filter not . elems <$> freeze visited

disintegrateEachBrick xs =
  map (disintegrateBrick xs) [1..snd (bounds xs)]

part =
  disintegrateEachBrick .
  reverseGraph .
  buildGraph .
  sortOn (snd . fst) .
  map parseLine

part1 = length . filter (== 0) . part
part2 = sum . part
