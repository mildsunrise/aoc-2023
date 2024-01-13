{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List (findIndices)
import qualified Data.Map as Map

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

stepOp 'L' = fst
stepOp 'R' = snd

parseNode [a1,a2,a3,' ','=',' ','(',b1,b2,b3,',',' ',c1,c2,c3,')'] =
  ([a1,a2,a3], ([b1,b2,b3], [c1,c2,c3]))

parseInput (steps : "" : nodes) =
  (map stepOp steps, Map.fromList $ map parseNode nodes)


-- PARTS

part1 = part (const ["AAA"])
part2 = part (filter ((== 'A') . last) . Map.keys)

part startNodes (parseInput -> (ops, net)) =
  foldl lcm 1 $
  map (inspectNode ops net) (startNodes net)

inspectNode ops net start =
  head $
  findIndices ((== 'Z') . last) $
  scanl (\node op -> op $ net Map.! node) start $
  cycle ops
