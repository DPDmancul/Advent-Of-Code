-- runghc 20-2.hs < 20.in.txt

{-
--- Part Two ---

The programs seem perplexed by your list of cheats. Apparently, the two-picosecond cheating rule was deprecated several milliseconds ago! The latest version of the cheating rule permits a single cheat that instead lasts at most 20 picoseconds.

Now, in addition to all the cheats that were possible in just two picoseconds, many more cheats are possible. This six-picosecond cheat saves 76 picoseconds:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#1#####.#.#.###
#2#####.#.#...#
#3#####.#.###.#
#456.E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Because this cheat has the same start and end positions as the one above, it's the same cheat, even though the path taken during the cheat is different:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S12..#.#.#...#
###3###.#.#.###
###4###.#.#...#
###5###.#.###.#
###6.E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Cheats don't need to use all 20 picoseconds; cheats can last any amount of time up to and including 20 picoseconds (but can still only end when the program is on normal track). Any cheat time not used is lost; it can't be saved for another cheat later.

You'll still need a list of the best cheats, but now there are even more to choose between. Here are the quantities of cheats in this example that save 50 picoseconds or more:

    There are 32 cheats that save 50 picoseconds.
    There are 31 cheats that save 52 picoseconds.
    There are 29 cheats that save 54 picoseconds.
    There are 39 cheats that save 56 picoseconds.
    There are 25 cheats that save 58 picoseconds.
    There are 23 cheats that save 60 picoseconds.
    There are 20 cheats that save 62 picoseconds.
    There are 19 cheats that save 64 picoseconds.
    There are 12 cheats that save 66 picoseconds.
    There are 14 cheats that save 68 picoseconds.
    There are 12 cheats that save 70 picoseconds.
    There are 22 cheats that save 72 picoseconds.
    There are 4 cheats that save 74 picoseconds.
    There are 3 cheats that save 76 picoseconds.

Find the best cheats using the updated cheating rules. How many cheats would save you at least 100 picoseconds?

-}

import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map qualified as Map

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
findCheats track = helper 0 best
  where
    best = fromJust $ race track
    bestMap = Map.fromList $ zip best [0..]
    helper _ [] = []
    helper n ((r, c) : xs) = map (subtract n) candidatePoses ++ helper (n + 1) xs
      where
        candidatePoses =
            mapMaybe
                (\(pos, d) -> subtract d <$> pos `Map.lookup` bestMap)
                [((r + i, c + j), abs i + abs j) | i <- [-20 .. 20], j <- [-20 + abs i .. 20 - abs i], i /= 0 || j /= 0]

main = interact $ show . length . filter (>= 100) . findCheats . parse
  where
    parse :: String -> Track
    parse s = foldr (uncurry helper) (Track Set.empty 0 0 (0, 0) (0, 0)) [(x, (r, c)) | (r, line) <- zip [0 ..] $ lines s, (c, x) <- zip [0 ..] line]
      where
        helper '#' pos@(r, c) i@Track{obstacles, rows, cols} = i{obstacles = Set.insert pos obstacles, rows = max rows (r + 1), cols = max cols (c + 1)}
        helper 'S' pos t = t{start = pos}
        helper 'E' pos t = t{end = pos}
        helper _ _ t = t
