{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeApplications, ViewPatterns #-}

import Numeric (readHex)
import Data.Bits (Bits((.&.)), shiftR)
import Data.Bifunctor (Bifunctor(first))

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseLine (words -> [[direction], amount, hex]) =
  ((direction, read @Int amount), parseHex hex)

parseHex ('(' : '#' : (readHex -> [(value, ")")])) =
  ("RDLU" !! (value .&. 0xF), value `shiftR` 4)


-- PARTS

toVector ('U', n) = ( 0,  n)
toVector ('D', n) = ( 0, -n)
toVector ('R', n) = ( n,  0)
toVector ('L', n) = (-n,  0)

polygonArea =
  sum .
  uncurry (zipWith (*)) .
  first (scanl (+) 0) .
  unzip

allTiles p = abs area + boundary `div` 2 + 1
  where
  area = polygonArea $ map toVector p
  boundary = sum $ map snd p

part f = allTiles . map (f . parseLine)
(part1, part2) = (part fst, part snd)
