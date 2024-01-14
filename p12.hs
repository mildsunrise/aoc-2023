{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications, TupleSections #-}

import Control.Applicative (Applicative(liftA2))
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import qualified Data.Vector as Vec
import qualified Data.Map as Map

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseLine (words ->
  [bits, map (read @Int) . splitOn "," -> runs]
  ) = (bits, runs)


-- PARTS

initialState = (0, Nothing)

nextState runs = helper
  where
  helper (n, Nothing) '#' = (\g -> (n, Just (g-1))) <$> getRun n
  helper (n, Nothing) '.' = Just (n, Nothing)
  helper (n, Just 0 ) '#' = Nothing
  helper (n, Just 0 ) '.' = Just (n+1, Nothing)
  helper (n, Just g ) '#' = Just (n, Just (g-1))
  helper (n, Just g ) '.' = Nothing

  getRun = (Vec.fromList runs Vec.!?)

isAccepting runs = (== length runs) . fst . endRun
  where
  endRun (n, Just 0) = (n+1, Nothing)
  endRun state       = state

expandBit '?' = ['.', '#']
expandBit c   = [c]

runNFA f states bs =
  Map.toList $
  Map.fromListWith (+) $
  catMaybes $
  liftA2 runEntry states bs
  where
  runEntry (state, i) b = (,i) <$> f state b

processLine (bits, runs) =
  sum $
  map snd $
  filter (isAccepting runs . fst) $
  foldl (runNFA (nextState runs)) [(initialState, 1)] $
  map expandBit bits

part f = sum . map (processLine . f . parseLine)

part1 = part id
part2 = part (bimap
  (intercalate "?" . replicate 5)
  (intercalate [] . replicate 5))
