-- runghc 16-1.hs < 16.in.txt

{-
--- Day 16: Reindeer Maze ---

It's time again for the Reindeer Olympics! This year, the big event is the Reindeer Maze, where the Reindeer compete for the lowest score.

You and The Historians arrive to search for the Chief right as the event is about to start. It wouldn't hurt to watch a little, right?

The Reindeer start on the Start Tile (marked S) facing East and need to reach the End Tile (marked E). They can move forward one tile at a time (increasing their score by 1 point), but never into a wall (#). They can also rotate clockwise or counterclockwise 90 degrees at a time (increasing their score by 1000 points).

To figure out the best place to sit, you start by grabbing a map (your puzzle input) from a nearby kiosk. For example:

###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############

There are many paths through this maze, but taking any of the best paths would incur a score of only 7036. This can be achieved by taking a total of 36 steps forward and turning 90 degrees a total of 7 times:

###############
#.......#....E#
#.#.###.#.###^#
#.....#.#...#^#
#.###.#####.#^#
#.#.#.......#^#
#.#.#####.###^#
#..>>>>>>>>v#^#
###^#.#####v#^#
#>>^#.....#v#^#
#^#.#.###.#v#^#
#^....#...#v#^#
#^###.#.#.#v#^#
#S..#.....#>>^#
###############

Here's a second example:

#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################

In this maze, the best paths cost 11048 points; following one such path would look like this:

#################
#...#...#...#..E#
#.#.#.#.#.#.#.#^#
#.#.#.#...#...#^#
#.#.#.#.###.#.#^#
#>>v#.#.#.....#^#
#^#v#.#.#.#####^#
#^#v..#.#.#>>>>^#
#^#v#####.#^###.#
#^#v#..>>>>^#...#
#^#v###^#####.###
#^#v#>>^#.....#.#
#^#v#^#####.###.#
#^#v#^........#.#
#^#v#^#########.#
#S#>>^..........#
#################

Note that the path shown above includes one 90 degree turn as the very first move, rotating the Reindeer from facing East to facing North.

Analyze your map carefully. What is the lowest score a Reindeer could possibly get?
-}

import Data.Coerce
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Set (Set)
import Data.Set qualified as Set

newtype PQ a = PQ (IntMap [a])

emptyPQ :: PQ a
emptyPQ = PQ IntMap.empty

singletonPQ :: Int -> a -> PQ a
singletonPQ p v = PQ $ IntMap.singleton p [v]

push :: Int -> a -> PQ a -> PQ a
push p v = coerce $ IntMap.insertWith (++) p [v]

pop :: PQ a -> Maybe ((Int, a), PQ a)
pop (PQ q) = helper <$> IntMap.minViewWithKey q
    where
    helper ((k, v : vs), q') = ((k, v), PQ $ if null vs then q' else IntMap.insert k vs q')

type Pos = (Int, Int)

data Dir = North | South | West | East
    deriving (Eq, Ord, Show)

rotateClockwise :: Dir -> Dir
rotateClockwise North = West
rotateClockwise South = East
rotateClockwise West = South
rotateClockwise East = North

rotateCounterclockwise :: Dir -> Dir
rotateCounterclockwise North = East
rotateCounterclockwise South = West
rotateCounterclockwise West = North
rotateCounterclockwise East = South

step :: Dir -> Pos -> Pos
step North (r, c) = (r - 1, c)
step South (r, c) = (r + 1, c)
step West (r, c) = (r, c - 1)
step East (r, c) = (r, c + 1)

data Info = Info
    { obstacles :: Set Pos
    , arrive :: Pos
    , next :: PQ (Pos, Dir)
    , visited :: Set (Pos, Dir)
    }

dijkstra :: Info -> Maybe Int
dijkstra i@Info{next, visited, arrive, obstacles}
    | Just ((cost, (pos, dir)), next') <- pop next =
        if pos == arrive
            then Just cost
            else
                let
                    moves =
                        filter
                            (\(_, (pos, dir)) -> pos `Set.notMember` obstacles && (pos, dir) `Set.notMember` visited)
                            [(cost + 1, (step dir pos, dir)), (cost + 1000, (pos, rotateClockwise dir)), (cost + 1000, (pos, rotateCounterclockwise dir))]
                 in
                    dijkstra i{visited = Set.insert (pos, dir) visited, next = foldr (uncurry push) next' moves}
    | otherwise = Nothing

main = interact $ show . dijkstra . parse
  where
    parse s = foldr id (Info Set.empty (0, 0) emptyPQ Set.empty) [helper x (r, c) | (r, line) <- zip [0 ..] $ lines s, (c, x) <- zip [0 ..] line]
      where
        helper '#' pos i@Info{obstacles} = i{obstacles = Set.insert pos obstacles}
        helper 'S' pos i = i{next = singletonPQ 0 (pos, East)}
        helper 'E' pos i = i{arrive = pos}
        helper _ _ i = i
