{-# LANGUAGE TypeApplications #-}

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARTS

parseSequence = map (read @Int) . words

diff x = zipWith (-) x (tail x)

extrapolate =
  sum .
  map head .
  takeWhile (any (/= 0)) .
  iterate diff .
  reverse

part f = sum . map (extrapolate . f . parseSequence)

part1 = part id
part2 = part reverse
