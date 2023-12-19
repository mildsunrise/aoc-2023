{-# LANGUAGE TupleSections #-}

import Data.List (elemIndices, sort)
import Data.Bifunctor (Bifunctor(bimap))

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARTS

part1 = part 1
part2 = part (1000000 - 1)

part k =
  uncurry (+) .
  bimap (sumAxis k . sort) (sumAxis k) .
  unzip .
  concat .
  zipWith galaxyPoints [0..]

galaxyPoints y = map (,y) . elemIndices '#'

sumAxis k xs =
  sum $
  zipWith (*) [1-l,3-l..] $
  expandAxis k xs
  where l = length xs

expandAxis k xs =
  zipWith (+) xs $
  map (* k) $
  scanl (+) (head xs) $
  map (max 0 . (+ (-1))) $
  zipWith (-) (tail xs) xs
