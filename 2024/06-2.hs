-- runghc 06-2.hs < 06.in.txt

{-
--- Part Two ---

While The Historians begin working around the guard's patrol route, you borrow their fancy device and step outside the lab. From the safety of a supply closet, you time travel through the last few months and record the nightly status of the lab's guard post on the walls of the closet.

Returning after what seems like only a few seconds to The Historians, they explain that the guard's patrol area is simply too large for them to safely search the lab without getting caught.

Fortunately, they are pretty sure that adding a single new obstruction won't cause a time paradox. They'd like to place the new obstruction in such a way that the guard will get stuck in a loop, making the rest of the lab safe to search.

To have the lowest chance of creating a time paradox, The Historians would like to know all of the possible positions for such an obstruction. The new obstruction can't be placed at the guard's starting position - the guard is there right now and would notice.

In the above example, there are only 6 different positions where a new obstruction would cause the guard to get stuck in a loop. The diagrams of these six situations use O to mark the new obstruction, | to show a position where the guard moves up/down, - to show a position where the guard moves left/right, and + to show a position where the guard moves both up/down and left/right.

Option one, put a printing press next to the guard's starting position:

....#.....
....+---+#
....|...|.
..#.|...|.
....|..#|.
....|...|.
.#.O^---+.
........#.
#.........
......#...

Option two, put a stack of failed suit prototypes in the bottom right quadrant of the mapped area:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
......O.#.
#.........
......#...

Option three, put a crate of chimney-squeeze prototype fabric next to the standing desk in the bottom right quadrant:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----+O#.
#+----+...
......#...

Option four, put an alchemical retroencabulator near the bottom left corner:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
..|...|.#.
#O+---+...
......#...

Option five, put the alchemical retroencabulator a bit to the right instead:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
....|.|.#.
#..O+-+...
......#...

Option six, put a tank of sovereign glue right next to the tank of universal solvent:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----++#.
#+----++..
......#O..

It doesn't really matter what you choose to use as an obstacle so long as you and The Historians can put it into position without the guard noticing. The important thing is having enough options that you can find one that minimizes time paradoxes, and in this example, there are 6 different positions you could choose.

You need to get the guard stuck in a loop by adding a single new obstruction. How many different positions could you choose for this obstruction?
-}

-- Set: 12 minutes
-- HashSet: 4 minutes

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (nub)

data Direction = North | South | West | East
    deriving (Eq, Enum)

type Pos = (Int, Int)

newtype HashSet a = HashSet IntSet

class Hashable a where
    hash :: a -> Int

emptySet :: HashSet a
emptySet = HashSet IntSet.empty

insert :: (Hashable a) => a -> HashSet a -> HashSet a
insert x (HashSet s) = HashSet $ IntSet.insert (hash x) s

member :: (Hashable a) => a -> HashSet a -> Bool
member x (HashSet s) = IntSet.member (hash x) s

notMember :: (Hashable a) => a -> HashSet a -> Bool
notMember x = not . member x

instance Hashable Pos where
    hash (x, y) = x * 2 ^ 15 + y

instance Hashable (Pos, Direction) where
    hash (pos, dir) = 4 * hash pos + fromEnum dir

data Status = Status
    { cols :: Int
    , rows :: Int
    , obstacles :: HashSet Pos
    , guardPos :: Pos
    , guardDir :: Direction
    }

outside :: Status -> Bool
outside Status{cols, rows, guardPos = (r, c)} = r < 1 || c < 1 || r > rows || c > cols

rotateRight :: Direction -> Direction
rotateRight North = East
rotateRight South = West
rotateRight West = North
rotateRight East = South

findNewObstacles :: Status -> [Pos]
findNewObstacles s@Status{obstacles, guardPos = startGuardPos} =
    filter cond $ nub $ findAllPos s
  where
    cond pos = pos /= startGuardPos && pos `notMember` obstacles && isLoop s{obstacles = insert pos obstacles} emptySet
    isLoop s posDirs
        | outside s = False
        | otherwise =
            let posDir = (guardPos s, guardDir s)
             in posDir `member` posDirs || isLoop (nextStep s) (insert posDir posDirs)

findAllPos :: Status -> [Pos]
findAllPos = map guardPos . takeWhile (not . outside) . iterate nextStep

advance :: Pos -> Direction -> Pos
advance (r, c) North = (r - 1, c)
advance (r, c) South = (r + 1, c)
advance (r, c) West = (r, c - 1)
advance (r, c) East = (r, c + 1)

nextStep :: Status -> Status
nextStep s@Status{obstacles, guardDir, guardPos} =
    let nextPos = advance guardPos guardDir
     in if nextPos `member` obstacles
            then s{guardDir = rotateRight guardDir}
            else s{guardPos = nextPos}

parse :: String -> Status
parse x =
    foldr
        helper
        (Status 0 0 emptySet (0, 0) North)
        [(r, c, char) | (r, line) <- zip [1 ..] (lines x), (c, char) <- zip [1 ..] line]
  where
    helper (r, c, '#') s@Status{obstacles} = helper (r, c, '.') s{obstacles = insert (r, c) obstacles}
    helper (r, c, '^') s = helper (r, c, '.') s{guardPos = (r, c), guardDir = North}
    helper (r, c, 'v') s = helper (r, c, '.') s{guardPos = (r, c), guardDir = South}
    helper (r, c, '<') s = helper (r, c, '.') s{guardPos = (r, c), guardDir = West}
    helper (r, c, '>') s = helper (r, c, '.') s{guardPos = (r, c), guardDir = East}
    helper (r, c, _) s@Status{rows, cols} = s{rows = max r rows, cols = max c cols}

main = interact $ show . length . findNewObstacles . parse
