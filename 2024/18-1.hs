-- runghc 18-1.hs < 18.in.txt

{-
--- Day 18: RAM Run ---

You and The Historians look a lot more pixelated than you remember. You're inside a computer at the North Pole!

Just as you're about to check out your surroundings, a program runs up to you. "This region of memory isn't safe! The User misunderstood what a pushdown automaton is and their algorithm is pushing whole bytes down on top of us! Run!"

The algorithm is fast - it's going to cause a byte to fall into your memory space once every nanosecond! Fortunately, you're faster, and by quickly scanning the algorithm, you create a list of which bytes will fall (your puzzle input) in the order they'll land in your memory space.

Your memory space is a two-dimensional grid with coordinates that range from 0 to 70 both horizontally and vertically. However, for the sake of example, suppose you're on a smaller grid with coordinates that range from 0 to 6 and the following list of incoming byte positions:

5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0

Each byte position is given as an X,Y coordinate, where X is the distance from the left edge of your memory space and Y is the distance from the top edge of your memory space.

You and The Historians are currently in the top left corner of the memory space (at 0,0) and need to reach the exit in the bottom right corner (at 70,70 in your memory space, but at 6,6 in this example). You'll need to simulate the falling bytes to plan out where it will be safe to run; for now, simulate just the first few bytes falling into your memory space.

As bytes fall into your memory space, they make that coordinate corrupted. Corrupted memory coordinates cannot be entered by you or The Historians, so you'll need to plan your route carefully. You also cannot leave the boundaries of the memory space; your only hope is to reach the exit.

In the above example, if you were to draw the memory space after the first 12 bytes have fallen (using . for safe and # for corrupted), it would look like this:

...#...
..#..#.
....#..
...#..#
..#..#.
.#..#..
#.#....

You can take steps up, down, left, or right. After just 12 bytes have corrupted locations in your memory space, the shortest path from the top left corner to the exit would take 22 steps. Here (marked with O) is one such path:

OO.#OOO
.O#OO#O
.OOO#OO
...#OO#
..#OO#.
.#.O#..
#.#OOOO

Simulate the first kilobyte (1024 bytes) falling onto your memory space. Afterward, what is the minimum number of steps needed to reach the exit?
-}

import Data.Coerce
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet

type Pos = (Int, Int)

posHash :: Pos -> Int
posHash (x, y) = 100 * x + y

posDehash :: Int -> Pos
posDehash x = (x `div` 100, x `mod` 100)

newtype PosSet = PosSet IntSet

emptySet :: PosSet
emptySet = PosSet IntSet.empty

singletonSet :: Pos -> PosSet
singletonSet = PosSet . IntSet.singleton . posHash

fromListSet :: [Pos] -> PosSet
fromListSet = PosSet . IntSet.fromList . map posHash

insertSet :: Pos -> PosSet -> PosSet
insertSet = coerce $ IntSet.insert . posHash

unionSet :: PosSet -> PosSet -> PosSet
unionSet = coerce IntSet.union

notMemberSet :: Pos -> PosSet -> Bool
notMemberSet = coerce $ IntSet.notMember . posHash

newtype PQ = PQ (IntMap PosSet)

emptyPQ :: PQ
emptyPQ = PQ IntMap.empty

singletonPQ :: Int -> Pos -> PQ
singletonPQ p v = PQ $ IntMap.singleton p (singletonSet v)

push :: Int -> Pos -> PQ -> PQ
push p v = coerce $ IntMap.insertWith unionSet p (singletonSet v)

pop :: PQ -> Maybe ((Int, Pos), PQ)
pop (PQ q) = helper <$> IntMap.minViewWithKey q
  where
    helper ((k, set :: PosSet), q')
        | Just (v, vs) <- IntSet.minView $ coerce set =
            ((k, posDehash v), PQ $ if IntSet.null vs then q' else IntMap.insert k (PosSet vs) q')

size = 70

bfs :: PosSet -> Maybe Int
bfs obstacles = helper emptySet $ singletonPQ 0 (0, 0)
  where
    helper visited next
        | Just ((cost, pos@(x, y)), next') <- pop next =
            if pos == (size, size)
                then Just cost
                else
                    let
                        moves =
                            filter
                                ( \pos@(x, y) ->
                                    pos `notMemberSet` obstacles
                                        && pos `notMemberSet` visited
                                        && x >= 0
                                        && x <= size
                                        && y >= 0
                                        && y <= size
                                )
                                [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                     in
                        helper (insertSet pos visited) $ foldr (push $ cost + 1) next' moves
        | otherwise = Nothing

main = interact $ show . bfs . fromListSet . take 1024 . map (\x -> read $ '(' : x ++ ")") . lines
