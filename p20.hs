{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections, ViewPatterns #-}

import Data.Bifunctor (Bifunctor(first))
import Data.Function (on)
import Data.List (unfoldr, partition, findIndex)
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
type WorldState = Map.Map String (NodeState, [String])

invertGraph =
  Map.fromListWith (++) .
  concatMap (\(node, (_, dests)) -> map (, [node]) dests)

initNode ins (node, (kind, dests)) = (node, (state, dests))
  where state = initState (ins Map.! node) kind

initWorld :: [String] -> WorldState
initWorld (map parseLine -> nodes) =
  Map.fromList $
  map (initNode $ invertGraph nodes) nodes

worldTick :: Pulse -> WorldState -> ([Pulse], WorldState)
worldTick (node, x) = Map.alterF helper node
  where
  helper Nothing = ([], Nothing)
  helper (Just (s, dests)) = (out, Just (s', dests))
    where
    (s', x') = handlePulse s x
    out = maybe [] (\x -> map (, (node, x)) dests) x'

runPulses :: Seq Pulse -> WorldState -> ([Pulse], WorldState)
runPulses (pulse :<| queue) states = let
  (out, states') = worldTick pulse states
  queue' = queue >< Seq.fromList out
  in first (pulse :) $ runPulses queue' states'
runPulses _ state = ([], state)


-- PART 1

sendPulse = runPulses . Seq.singleton
pulseStream pulse = unfoldr (Just . sendPulse pulse)

part1 =
  uncurry (on (*) length) .
  partition (snd . snd) .
  concat .
  take 1000 .
  pulseStream ("broadcaster", (undefined, False)) .
  initWorld


-- PART 2

subgraphCycle world root = n + 1
  where
  [accum] = invertGraph (Map.toList world) Map.! "rx"
  pulse = (root, ("broadcaster", False))
  event = any (snd . snd) . filter ((== accum) . fst)
  Just n = findIndex event (pulseStream pulse world)

firstOutLow world =
  product $
  map (subgraphCycle world) $
  snd (world Map.! "broadcaster")

part2 = firstOutLow . initWorld
