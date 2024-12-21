-- runghc 15-2.hs < 15.in.txt

{-
--- Part Two ---

The lanternfish use your information to find a safe moment to swim in and turn off the malfunctioning robot! Just as they start preparing a festival in your honor, reports start coming in that a second warehouse's robot is also malfunctioning.

This warehouse's layout is surprisingly similar to the one you just helped. There is one key difference: everything except the robot is twice as wide! The robot's list of movements doesn't change.

To get the wider warehouse's map, start with your original map and, for each tile, make the following changes:

    If the tile is #, the new map contains ## instead.
    If the tile is O, the new map contains [] instead.
    If the tile is ., the new map contains .. instead.
    If the tile is @, the new map contains @. instead.

This will produce a new warehouse map which is twice as wide and with wide boxes that are represented by []. (The robot does not change size.)

The larger example from before would now look like this:

####################
##....[]....[]..[]##
##............[]..##
##..[][]....[]..[]##
##....[]@.....[]..##
##[]##....[]......##
##[]....[]....[]..##
##..[][]..[]..[][]##
##........[]......##
####################

Because boxes are now twice as wide but the robot is still the same size and speed, boxes can be aligned such that they directly push two other boxes at once. For example, consider this situation:

#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^

After appropriately resizing this map, the robot would push around these boxes as follows:

Initial state:
##############
##......##..##
##..........##
##....[][]@.##
##....[]....##
##..........##
##############

Move <:
##############
##......##..##
##..........##
##...[][]@..##
##....[]....##
##..........##
##############

Move v:
##############
##......##..##
##..........##
##...[][]...##
##....[].@..##
##..........##
##############

Move v:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##.......@..##
##############

Move <:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##......@...##
##############

Move <:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##.....@....##
##############

Move ^:
##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############

Move ^:
##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############

Move <:
##############
##......##..##
##...[][]...##
##....[]....##
##....@.....##
##..........##
##############

Move <:
##############
##......##..##
##...[][]...##
##....[]....##
##...@......##
##..........##
##############

Move ^:
##############
##......##..##
##...[][]...##
##...@[]....##
##..........##
##..........##
##############

Move ^:
##############
##...[].##..##
##...@.[]...##
##....[]....##
##..........##
##..........##
##############

This warehouse also uses GPS to locate the boxes. For these larger boxes, distances are measured from the edge of the map to the closest edge of the box in question. So, the box shown below has a distance of 1 from the top edge of the map and 5 from the left edge of the map, resulting in a GPS coordinate of 100 * 1 + 5 = 105.

##########
##...[]...
##........

In the scaled-up version of the larger example from above, after the robot has finished all of its moves, the warehouse would look like this:

 01234567890123456789
0#################### 0
1##[].......[].[][]## 1
2##[]...........[].## 2
3##[]........[][][]## 3
4##[]......[]....[]## 4
5##..##......[]....## 5
6##..[]............## 6
7##..@......[].[][]## 7
8##......[][]..[]..## 8
9#################### 9
 01234567890123456789

The sum of these boxes' GPS coordinates is 9021.

Predict the motion of the robot and boxes in this new, scaled-up warehouse. What is the sum of all boxes' final GPS coordinates?
-}

import Data.Bifunctor
import Data.Coerce
import Data.Foldable
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet

data Dir = North | South | West | East
    deriving(Eq)

type Pos = (Int, Int)

gps :: Pos -> Int
gps (r, c) = 100 * r + c

newtype PosSet = PosSet {gpsSet :: IntSet}

emptySet :: PosSet
emptySet = PosSet IntSet.empty

insert :: Pos -> PosSet -> PosSet
insert = coerce IntSet.insert . gps

delete :: Pos -> PosSet -> PosSet
delete = coerce IntSet.delete . gps

member :: Pos -> PosSet -> Bool
member = coerce IntSet.member . gps

notMember :: Pos -> PosSet -> Bool
notMember = (not .) . member

data Warehouse = Warehouse
    { obstacles :: PosSet
    , boxes :: PosSet
    , robot :: Pos
    }

delta :: Dir -> Pos
delta North = (-1, 0)
delta South = (1, 0)
delta West = (0, -1)
delta East = (0, 1)

add :: Pos -> Pos -> Pos
add (xa, ya) (xb, yb) = (xa + xb, ya + yb)

move :: Dir -> Warehouse -> Warehouse
move dir w@Warehouse{robot} =
    let newRobot = robot `add` moveVector
     in maybe w (\boxes -> w{boxes, robot = newRobot}) $ helper newRobot w
  where
    moveVector = delta dir
    left = add (0, -1)
    right = add (0, 1)
    helper pos Warehouse{obstacles} | pos `member` obstacles = Nothing
    helper pos Warehouse{boxes}
        | pos `member` boxes = moveBox id
        | left pos `member` boxes = moveBox left
        | otherwise = Just boxes
      where
        moveBox f =
            let newPos = f pos `add` moveVector
             in insert newPos <$> do
                let boxes' = delete (f pos) boxes
                boxes'' <- if dir == East then Just boxes' else moveHalfBox newPos boxes'
                if dir == West then Just boxes'' else moveHalfBox (right newPos) boxes''
        moveHalfBox pos boxes = helper pos w{boxes = boxes}

main = interact $ show . sum . IntSet.elems . gpsSet . boxes . uncurry (foldl' $ flip move) . parse
  where
    parse = bimap parseWarehouse parseMoves . break null . lines
    parseWarehouse lines = foldr id (Warehouse emptySet emptySet (0, 0)) [helper x (r, c) | (r, line) <- zip [0 ..] lines, (c, x) <- zip [0 ..] line]
      where
        helper '#' (r, c) w@Warehouse{obstacles} = w{obstacles = insert (r, 2 * c) $ insert (r, 2 * c + 1) obstacles}
        helper 'O' (r, c) w@Warehouse{boxes} = w{boxes = insert (r, 2 * c) boxes}
        helper '@' (r, c) w = w{robot = (r, 2 * c)}
        helper _ _ w = w
    parseMoves = map parseMove . filter (`elem` "^v<>") . concat
    parseMove '^' = North
    parseMove 'v' = South
    parseMove '<' = West
    parseMove '>' = East
