import Data.List (tails, transpose, findIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Control.Applicative (Alternative ((<|>)))

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARTS

lineDiffs a b =
  length $
  filter (uncurry (/=)) $
  zip a b

findReflection f xs =
  fmap (+ 1) $
  findIndex f $
  drop 1 $
  init $
  zipWith (zipWith lineDiffs) (tails xs) $
  scanl (flip (:)) [] xs

summarize f xs = fromJust (vline <|> hline)
  where
  vline = (* 100) <$> findReflection f xs
  hline = findReflection f (transpose xs)

part f =
  sum .
  map (summarize f) .
  splitOn [""]

part1 = part (all (== 0))
part2 = part ((== [1]) . filter (/= 0))
