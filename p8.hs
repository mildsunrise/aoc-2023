{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TupleSections #-}

import Data.Maybe (catMaybes, fromMaybe)
import Data.Bifunctor (Bifunctor(bimap, first))
import Data.List (findIndices)
import Control.Applicative (Applicative(liftA2))
import qualified Data.Map as Map
import qualified Data.Set as Set

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

stepOp 'L' = fst
stepOp 'R' = snd

parseNode [a1,a2,a3,' ','=',' ','(',b1,b2,b3,',',' ',c1,c2,c3,')'] =
  ([a1,a2,a3], ([b1,b2,b3], [c1,c2,c3]))

parseInput (steps : "" : nodes) =
  (map stepOp steps, Map.fromList $ map parseNode nodes)


-- GENERIC ALGORITHMS

findCycle = (`helper` Map.empty) . zip [0..]
  where
  helper ((i,x):xs) m =
    either (, Map.size m) (helper xs) $
    Map.alterF (maybe (Right $ Just i) Left) x m

egcd a b = helper (a, 1, 0) (b, 0, 1)
  where
  helper a (0, _, _) = a
  helper a b = helper b (step a b)

  step (a, aa, ab) (b, ba, bb) = let
    (q, r) = (div a b, mod a b)
    f a b = a - b * q
    in (r, f aa ba, f ba bb)

intersect (a, ma) (b, mb)
  | (b - a) `mod` d == 0 = Just (c `mod` mc, mc)
  | otherwise            = Nothing
  where
  (d, x, _) = egcd ma mb
  c = a + ((b - a) `div` d) * (x * ma)
  mc = ma * mb `div` d


-- STAGE 1: Inspect the graph

part1 = part (const ["AAA"])
part2 = part (filter ((== 'A') . last) . Map.keys)

part startNodes (parseInput -> (ops, net)) =
  earliestCandidate $
  map (inspectNode ops net) $
  startNodes net

inspectNode ops net start = ((a, b), candidates)
  where
  l = length ops
  followOp node op = op $ net Map.! node

  (a, b) =
    bimap (* l) (* l) $
    findCycle $
    iterate (\node -> foldl followOp node ops) $
    start

  candidates =
    findIndices ((== 'Z') . last) $
    take b $
    scanl followOp start $
    cycle ops


-- STAGE 2: Math Stuff(tm)

earliestCandidate nodeData = fromMaybe periodicCandidate commonCandidate
  where
  commonPeriod = maximum $ map (\((a,b),_) -> a) nodeData
  commonCandidates (_,candidates) =
    Set.fromAscList $
    take commonPeriod candidates
  commonCandidate =
    Set.lookupMin $
    foldl1 Set.intersection $
    map commonCandidates nodeData

  periodicCandidates ((a,b),candidates) =
    map (, b - a) $
    dropWhile (< a) candidates
  normalizeCandidate =
    (+ commonPeriod) .
    uncurry mod .
    first (+ (-commonPeriod))
  periodicCandidate =
    minimum $
    map normalizeCandidate $
    foldl1 (\a b -> catMaybes $ liftA2 intersect a b) $
    map periodicCandidates nodeData
