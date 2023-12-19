{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications #-}

import Data.Maybe (fromJust)
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (group, sort, sortOn, elemIndex, partition)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseBid (words -> [hand, read @Int -> bid]) = (hand, bid)


-- PART 1

handType = reverse . sort . map length . group . sort

cardStrength = fromJust . (`elemIndex` "23456789TJQKA")

handStrength x = (handType x, map cardStrength x)

part f =
  sum .
  zipWith (*) [1..] .
  map snd .
  sortOn (f . fst) .
  map parseBid

part1 = part handStrength


-- PART 2

zipWithRest f (a:as) (b:bs) = f a b : zipWithRest f as bs
zipWithRest _ []     bs     = bs
zipWithRest _ as     []     = as

handType2 =
  uncurry (zipWithRest (+)) .
  bimap handType handType .
  partition (== 'J')

cardStrength2 'J' = -1
cardStrength2 x   = cardStrength x

handStrength2 x = (handType2 x, map cardStrength2 x)

part2 = part handStrength2
