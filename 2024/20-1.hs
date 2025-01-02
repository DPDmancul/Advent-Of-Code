-- runghc 20-1.hs < 20.in.txt

{-
--- Day 20: Race Condition ---

The Historians are quite pixelated again. This time, a massive, black building looms over you - you're right outside the CPU!

While The Historians get to work, a nearby program sees that you're idle and challenges you to a race. Apparently, you've arrived just in time for the frequently-held race condition festival!

The race takes place on a particularly long and twisting code path; programs compete to see who can finish in the fewest picoseconds. The winner even gets their very own mutex!

They hand you a map of the racetrack (your puzzle input). For example:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

The map consists of track (.) - including the start (S) and end (E) positions (both of which also count as track) - and walls (#).

When a program runs through the racetrack, it starts at the start position. Then, it is allowed to move up, down, left, or right; each such move takes 1 picosecond. The goal is to reach the end position as quickly as possible. In this example racetrack, the fastest time is 84 picoseconds.

Because there is only a single path from the start to the end and the programs all go the same speed, the races used to be pretty boring. To make things more interesting, they introduced a new rule to the races: programs are allowed to cheat.

The rules for cheating are very strict. Exactly once during a race, a program may disable collision for up to 2 picoseconds. This allows the program to pass through walls as if they were regular track. At the end of the cheat, the program must be back on normal track again; otherwise, it will receive a segmentation fault and get disqualified.

So, a program could complete the course in 72 picoseconds (saving 12 picoseconds) by cheating for the two moves marked 1 and 2:

###############
#...#...12....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Or, a program could complete the course in 64 picoseconds (saving 20 picoseconds) by cheating for the two moves marked 1 and 2:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...12..#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

This cheat saves 38 picoseconds:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.####1##.###
#...###.2.#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

This cheat saves 64 picoseconds and takes the program directly to the end:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..21...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Each cheat has a distinct start position (the position where the cheat is activated, just before the first move that is allowed to go through walls) and end position; cheats are uniquely identified by their start position and end position.

In this example, the total number of cheats (grouped by the amount of time they save) are as follows:

    There are 14 cheats that save 2 picoseconds.
    There are 14 cheats that save 4 picoseconds.
    There are 2 cheats that save 6 picoseconds.
    There are 4 cheats that save 8 picoseconds.
    There are 2 cheats that save 10 picoseconds.
    There are 3 cheats that save 12 picoseconds.
    There is one cheat that saves 20 picoseconds.
    There is one cheat that saves 36 picoseconds.
    There is one cheat that saves 38 picoseconds.
    There is one cheat that saves 40 picoseconds.
    There is one cheat that saves 64 picoseconds.

You aren't sure what the conditions of the racetrack will be like, so to give yourself as many options as possible, you'll need a list of the best cheats. How many cheats would save you at least 100 picoseconds?
-}

import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

type Pos = (Int, Int)

data Track = Track {obstacles :: Set Pos, rows :: Int, cols :: Int, start :: Pos, end :: Pos}

nothingIfNull :: (Traversable t) => t a -> Maybe (t a)
nothingIfNull x | null x = Nothing
nothingIfNull x = Just x

race :: Track -> Maybe [Pos]
race = helper Set.empty
 where
  helper visited Track{start, end} | start == end = Just [end]
  helper visited t@Track{obstacles, rows, cols, start = start@(r, c)} =
    (start :) . minimumBy (on compare length)
      <$> nothingIfNull (mapMaybe (\x -> helper (Set.insert start visited) t{start = x}) $ filter isValid neighbours)
   where
    neighbours = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
    isValid x@(r, c) = x `Set.notMember` obstacles && x `Set.notMember` visited && r >= 0 && r < rows && c >= 0 && c < cols

findCheats :: Track -> [Int]
findCheats track = helper 2 best
 where
  best = fromJust $ race track
  helper _ [] = []
  helper n ((r, c) : xs) = map (subtract n) candidatePoses ++ helper (n + 1) xs
   where
    candidatePoses = mapMaybe (`elemIndex` best) [(r + 2, c), (r - 2, c), (r, c + 2), (r, c - 2)]

main = interact $ show . length . filter (>= 100) . findCheats . parse
 where
  parse :: String -> Track
  parse s = foldr (uncurry helper) (Track Set.empty 0 0 (0, 0) (0, 0)) [(x, (r, c)) | (r, line) <- zip [0 ..] $ lines s, (c, x) <- zip [0 ..] line]
   where
    helper '#' pos@(r, c) i@Track{obstacles, rows, cols} = i{obstacles = Set.insert pos obstacles, rows = max rows (r + 1), cols = max cols (c + 1)}
    helper 'S' pos t = t{start = pos}
    helper 'E' pos t = t{end = pos}
    helper _ _ t = t
