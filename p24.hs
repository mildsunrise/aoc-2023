{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications #-}

import Data.Maybe (isJust)
import Data.List (tails, uncons, transpose)
import Data.List.Split (splitOn)
import Control.Monad (guard)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

solve k xs =
  fmap (map (\[a] -> a) . fst) $
  iterate (>>= solveStep) (Just ([], xs)) !! k

solveStep (as, xs) = do
  (bs, xs) <- pure $ span ((== 0) . head) xs
  (k:p, xs) <- uncons xs
  p <- pure $ map (* (1/k)) p
  let f (k:r) = zipWith (-) r $ map (* k) p
  pure (map f as ++ [p], map f (bs ++ xs))

pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

det2 [[a, b], [c, d]] = a * d - b * c


-- PARSING

parseVec = map (read @Integer) . splitOn ","

parseLine f (
  map (f . parseVec) . splitOn "@" -> [pos, vel]
  ) = (pos, vel)


-- PART 1

inTestArea x = 2 * 10^14 <= x && x <= 4 * 10^14

isIntersecting ((pa, va), (pb, vb)) = isJust $ do
  let mat = [va, map negate vb, zipWith (-) pb pa]
  [ka, kb] <- solve 2 (transpose mat)
  let point = zipWith (+) pa $ map (* ka) va
  guard $ ka >= 0 && kb >= 0 && all inTestArea point

rationalVec = map (fromInteger @Rational)

part1 =
  length .
  filter isIntersecting .
  pairs .
  map (parseLine (take 2 . rationalVec))


-- PART 2

choose2 [a, b, c] = [
  (\[x, y] -> [x, y, 0], [a, b]),
  (\[x, y] -> [x, 0, y], [a, c]),
  (\[x, y] -> [0, x, y], [b, c])]

minors2 = map (det2 . snd) . choose2 . transpose

produceEqs (pb, vb) (pc, vc) = let
  cols = zip (zipWith (-) vb vc) (zipWith (-) pc pb)
  ks = zipWith (-) (minors2 [pb, vb]) (minors2 [pc, vc])
  in zipWith produceEq ks $ choose2 cols

produceEq k (build, rows) = let
  f [a, b] = build [b, -a]
  (vd, pd) = unzip rows
  in f vd ++ f pd ++ [k]

part2 =
  sum .
  (\(Just xs) -> take 3 xs) .
  solve 6 .
  (\(x:xs) -> concatMap (produceEqs x) xs) .
  map (parseLine rationalVec)
