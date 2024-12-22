-- runghc 16-2.hs < 16.in.txt

{-
--- Part Two ---

Now that you know what the best paths look like, you can figure out the best spot to sit.

Every non-wall tile (S, ., or E) is equipped with places to sit along the edges of the tile. While determining which of these tiles would be the best spot to sit depends on a whole bunch of factors (how comfortable the seats are, how far away the bathrooms are, whether there's a pillar blocking your view, etc.), the most important factor is whether the tile is on one of the best paths through the maze. If you sit somewhere else, you'd miss all the action!

So, you'll need to determine which tiles are part of any best path through the maze, including the S and E tiles.

In the first example, there are 45 tiles (marked O) that are part of at least one of the various best paths through the maze:

###############
#.......#....O#
#.#.###.#.###O#
#.....#.#...#O#
#.###.#####.#O#
#.#.#.......#O#
#.#.#####.###O#
#..OOOOOOOOO#O#
###O#O#####O#O#
#OOO#O....#O#O#
#O#O#O###.#O#O#
#OOOOO#...#O#O#
#O###.#.#.#O#O#
#O..#.....#OOO#
###############

In the second example, there are 64 tiles that are part of at least one of the best paths:

#################
#...#...#...#..O#
#.#.#.#.#.#.#.#O#
#.#.#.#...#...#O#
#.#.#.#.###.#.#O#
#OOO#.#.#.....#O#
#O#O#.#.#.#####O#
#O#O..#.#.#OOOOO#
#O#O#####.#O###O#
#O#O#..OOOOO#OOO#
#O#O###O#####O###
#O#O#OOO#..OOO#.#
#O#O#O#####O###.#
#O#O#OOOOOOO..#.#
#O#O#O#########.#
#O#OOO..........#
#################

Analyze your map further. How many tiles are part of at least one of the best paths through the maze?
-}

import Data.Bifunctor
import Data.Coerce
import Data.Function
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List
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
    , next :: PQ (Pos, Dir, [Pos])
    , visited :: Set (Pos, Dir)
    }

dijkstra :: Info -> [(Int, [Pos])]
dijkstra i@Info{next, visited, arrive, obstacles}
    | Just ((cost, (pos, dir, path)), next') <- pop next =
        if pos == arrive
            then (cost, pos : path) : dijkstra i{visited = Set.insert (pos, dir) visited, next = next'}
            else
                let
                    stepped = step dir pos
                    moves =
                        filter
                            (\(_, (pos, dir, _)) -> pos `Set.notMember` obstacles && (pos, dir) `Set.notMember` visited)
                            [ (cost + 1, (stepped, dir, stepped : path))
                            , (cost + 1000, (pos, rotateClockwise dir, path))
                            , (cost + 1000, (pos, rotateCounterclockwise dir, path))
                            ]
                 in
                    dijkstra i{visited = Set.insert (pos, dir) visited, next = foldr (uncurry push) next' moves}
    | otherwise = []

main = interact $ show . (1+) . length . nub . concatMap snd . head . groupBy (on (==) fst) . sortBy (on compare fst) . dijkstra . parse
  where
    parse s = foldr id (Info Set.empty (0, 0) emptyPQ Set.empty) [helper x (r, c) | (r, line) <- zip [0 ..] $ lines s, (c, x) <- zip [0 ..] line]
      where
        helper '#' pos i@Info{obstacles} = i{obstacles = Set.insert pos obstacles}
        helper 'S' pos i = i{next = singletonPQ 0 (pos, East, [])}
        helper 'E' pos i = i{arrive = pos}
        helper _ _ i = i
