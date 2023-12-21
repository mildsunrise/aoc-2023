{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications, TupleSections #-}

import Data.Bifunctor (Bifunctor(second))
import Data.Foldable (Foldable(toList))
import Data.List.Split (splitOn)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseAction (splitOn "-" -> [k, ""]) = (k, Nothing)
parseAction (splitOn "=" -> [k, read @Int -> v]) = (k, Just v)

splitActions = splitOn "," . head


-- PART 1

hash = foldl step 0 . map fromEnum
  where
  step a c = (`mod` 256) $ (* 17) $ c + a

part1 = sum . map hash . splitActions


-- PART 2

runAction table (k, v) =
  MVec.modify table op (hash k)
  where
  prefix = toList ((k,) <$> v)
  op =
    uncurry (++) .
    second ((prefix ++) . drop 1) .
    break ((== k) . fst)

runActions actions = toList $ Vec.create $ do
  table <- MVec.replicate 256 []
  table <$ mapM (runAction table) actions

part2 =
  summarize .
  map (summarize . map snd) .
  runActions .
  map parseAction .
  splitActions
  where
  summarize = sum . zipWith (*) [1..]
