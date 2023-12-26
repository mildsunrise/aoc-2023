{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections, ViewPatterns, BangPatterns #-}

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)), (><))

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseName "broadcaster" = ("broadcaster", Nothing)
parseName (c:n)         = (n, Just c)

parseLine (splitOn " -> " ->
  [ parseName -> (name, kind), splitOn ", " -> dests ]
  ) = (name, (kind, dests))


-- NODE IMPLEMENTATION

data NodeState =
  Broadcaster |
  FlipFlop Bool |
  Conjunction (Map.Map String Bool)
  deriving (Eq, Ord, Show)

initState _   Nothing    = Broadcaster
initState _   (Just '%') = FlipFlop False
initState ins (Just '&') = Conjunction s
  where s = Map.fromList $ map (, False) ins

handlePulse Broadcaster (_, x) = (Broadcaster, Just x)
handlePulse (FlipFlop s) (_, False) = (FlipFlop s', Just s')
  where s' = not s
handlePulse (FlipFlop s) (_, True) = (FlipFlop s, Nothing)
handlePulse (Conjunction s) (from, x) = (Conjunction s', Just x')
  where
  s' = Map.insert from x s
  x' = not $ and $ Map.elems s'


-- WORLD IMPLEMENTATION

type Pulse = (String, (String, Bool))
type WorldNodes = Map.Map String (NodeState, [String])
type WorldState = ((Int, Int), WorldNodes, Seq Pulse)

updateStats (_, (_, False)) (!a, !b) = (a + 1, b)
updateStats (_, (_, True )) (!a, !b) = (a, b + 1)

invertGraph =
  Map.fromListWith (++) .
  concatMap (\(node, (_, dests)) -> map (, [node]) dests)

initNode ins (node, (kind, dests)) = (node, (state, dests))
  where state = initState (ins Map.! node) kind

initWorld :: [String] -> WorldState
initWorld =
  ((0, 0),, Seq.empty) .
  Map.fromList .
  (\ns -> map (initNode $ invertGraph ns) ns) .
  map parseLine

worldTick :: Pulse -> WorldNodes -> ([Pulse], WorldNodes)
worldTick (node, x) = Map.alterF helper node
  where
  helper Nothing = ([], Nothing)
  helper (Just (s, dests)) = (out, Just (s', dests))
    where
    (s', x') = handlePulse s x
    out = maybe [] (\x -> map (, (node, x)) dests) x'

runPulses :: WorldState -> WorldState
runPulses (stats, states, pulse :<| queue) = let
  !stats' = updateStats pulse stats
  (out, states') = worldTick pulse states
  queue' = queue >< Seq.fromList out
  in runPulses (stats', states', queue')
runPulses state = state


-- PART 1

buttonPulse = ("broadcaster", (undefined, False))

buttonPush (stats, states, queue) =
  runPulses (stats, states, buttonPulse :<| queue)

part1 =
  uncurry (*) .
  (\(s, _, _) -> s) .
  (!! 1000) .
  iterate buttonPush .
  initWorld

part2 = const "TODO"
