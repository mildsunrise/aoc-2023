import Data.List (isPrefixOf, findIndex, tails)
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- CONSTANTS

digits = ["zero","one","two","three","four",
          "five","six","seven","eight","nine"]


-- PART 1

lineToValue =
  read .
  (\s -> [head s, last s]) .
  filter isDigit

part1 = sum . map lineToValue


-- PART 2

matchDigit (c:_) | isDigit c = Just c
matchDigit s = (['0'..] !!) <$> findIndex (`isPrefixOf` s) digits

replaceDigits = mapMaybe matchDigit . tails

part2 = part1 . map replaceDigits
