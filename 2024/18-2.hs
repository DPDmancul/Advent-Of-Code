-- runghc 18-2.hs < 18.in.txt

{-
--- Part Two ---

The Historians aren't as used to moving around in this pixelated universe as you are. You're afraid they're not going to be fast enough to make it to the exit before the path is completely blocked.

To determine how fast everyone needs to go, you need to determine the first byte that will cut off the path to the exit.

In the above example, after the byte at 1,1 falls, there is still a path to the exit:

O..#OOO
O##OO#O
O#OO#OO
OOO#OO#
###OO##
.##O###
#.#OOOO

However, after adding the very next byte (at 6,1), there is no longer a path to the exit:

...#...
.##..##
.#..#..
...#..#
###..##
.##.###
#.#....

So, in this example, the coordinates of the first byte that prevents the exit from being reachable are 6,1.

Simulate more of the bytes that are about to corrupt your memory space. What are the coordinates of the first byte that will prevent the exit from being reachable from your starting position? (Provide the answer as two integers separated by a comma with no other characters.)
-}

import Data.Coerce
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List
import Data.Maybe

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

main = interact $ show . fmap last . find (isNothing . bfs . fromListSet) . drop 1024 . inits . map (\x -> read $ '(' : x ++ ")") . lines
