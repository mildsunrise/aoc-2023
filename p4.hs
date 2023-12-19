{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications #-}

import Data.List.Split (splitOn)
import qualified Data.Set as Set

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseSet = Set.fromList . map (read @Int) . words

parseCard (splitOn ":" ->
  [_, map parseSet . splitOn "|" -> [winners, mine]]
  ) = (winners, mine)


-- PARTS

cardCount = Set.size . uncurry Set.intersection

countToPoints 0 = 0
countToPoints n = 2 ^ (n-1)

cardTotals count totals =
  (: totals) $
  (1 +) $
  sum $
  take count totals

part f = sum . f . map (cardCount . parseCard)

part1 = part (map countToPoints)
part2 = part (foldr cardTotals [])
