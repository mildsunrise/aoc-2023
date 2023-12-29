{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections, TypeApplications, ViewPatterns #-}

import Control.Monad (forM, forM_)
import Control.Monad.ST.Strict (ST, runST)
import Data.Array (bounds, (!), Ix (inRange, range), accumArray)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
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


-- PART 1

buildGraph xs = runST $ do
  let
    xyStat f g = bimap f f $ unzip $ map (fst . g) xs
    bs = (xyStat minimum fst, xyStat maximum snd)
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

disintegrablePieces xs =
  ((length xs + 1) -) $
  length $
  nubOrd $
  map head $
  filter ((== 1) . length) $
  map snd xs

part1 =
  disintegrablePieces .
  buildGraph .
  sortOn (snd . fst) .
  map parseLine


-- PART 2

part2 = const "TODO"
