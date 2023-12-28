{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications #-}

import Data.Maybe (isJust)
import Data.List (tails, uncons, transpose)
import Data.List.Split (splitOn)
import Control.Monad (guard)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

solve n xs =
  iterate (>>= solveStep) (Just ([], xs)) !! n

solveStep (as, xs) = do
  (bs, xs) <- pure $ span ((== 0) . head) xs
  (k:c, xs) <- uncons xs
  c <- pure $ map (* (1/k)) c
  let f (k:r) = zipWith (-) r $ map (* k) c
  pure (map f as ++ [c], map f (bs ++ xs))

pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]


-- PARSING

parseVec = map (read @Integer) . splitOn ","

parseLine f (
  map (f . parseVec) . splitOn "@" -> [pos, vel]
  ) = (pos, vel)


-- PART 1

inTestArea x = 2 * 10^14 <= x && x <= 4 * 10^14

isIntersecting ((pa, va), (pb, vb)) = isJust $ do
  let mat = [va, map negate vb, zipWith (-) pb pa]
  ([[ka], [kb]], []) <- solve 2 (transpose mat)
  let point = zipWith (+) pa $ map (* ka) va
  guard $ ka >= 0 && kb >= 0 && all inTestArea point

rationalVec = map (fromInteger @Rational)

part1 =
  length .
  filter isIntersecting .
  pairs .
  map (parseLine (take 2 . rationalVec))


-- PART 2

part2 = const "TODO"
