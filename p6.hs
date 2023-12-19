{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications #-}

import Data.List.Split (splitOn)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseLine f (splitOn ":" ->
  [label, map (read @Int) . f . words -> values]
  ) = (label, values)

parseRaces f (map (parseLine f) ->
  [("Time", times), ("Distance", distances)]
  ) = zip times distances


-- PARTS

waysToWin (t, d) =
  (+ (-1)) $
  (+ (t `mod` 2)) $
  (* 2) $
  ceiling $
  (/ 2) $
  (+ fromIntegral (-(t `mod` 2))) $
  sqrt $
  fromIntegral $
  t^2 - 4*d

part f = product . map waysToWin . parseRaces f

part1 = part id
part2 = part ((: []) . concat)
