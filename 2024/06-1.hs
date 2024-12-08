-- runghc 06-1.hs < 06.in.txt

{-
--- Day 6: Guard Gallivant ---

The Historians use their fancy device again, this time to whisk you all away to the North Pole prototype suit manufacturing lab... in the year 1518! It turns out that having direct access to history is very convenient for a group of historians.

You still have to be careful of time paradoxes, and so it will be important to avoid anyone from 1518 while The Historians search for the Chief. Unfortunately, a single guard is patrolling this part of the lab.

Maybe you can work out where the guard will go ahead of time so that The Historians can search safely?

You start by making a map (your puzzle input) of the situation. For example:

....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...

The map shows the current position of the guard with ^ (to indicate the guard is currently facing up from the perspective of the map). Any obstructions - crates, desks, alchemical reactors, etc. - are shown as #.

Lab guards in 1518 follow a very strict patrol protocol which involves repeatedly following these steps:

    If there is something directly in front of you, turn right 90 degrees.
    Otherwise, take a step forward.

Following the above protocol, the guard moves up several times until she reaches an obstacle (in this case, a pile of failed suit prototypes):

....#.....
....^....#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...

Because there is now an obstacle in front of the guard, she turns right before continuing straight in her new facing direction:

....#.....
........>#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...

Reaching another obstacle (a spool of several very long polymers), she turns right again and continues downward:

....#.....
.........#
..........
..#.......
.......#..
..........
.#......v.
........#.
#.........
......#...

This process continues for a while, but the guard eventually leaves the mapped area (after walking past a tank of universal solvent):

....#.....
.........#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#v..

By predicting the guard's route, you can determine which specific positions in the lab will be in the patrol path. Including the guard's starting position, the positions visited by the guard before leaving the area are marked with an X:

....#.....
....XXXXX#
....X...X.
..#.X...X.
..XXXXX#X.
..X.X.X.X.
.#XXXXXXX.
.XXXXXXX#.
#XXXXXXX..
......#X..

In this example, the guard will visit 41 distinct positions on your map.

Predict the path of the guard. How many distinct positions will the guard visit before leaving the mapped area?
-}

import Data.List

data Direction = North | South | West | East

type Pos = (Int, Int)

data Status = Status
    { cols :: Int
    , rows :: Int
    , obstacles :: [Pos]
    , guardPos :: Pos
    , guardDir :: Direction
    }

outside :: Status -> Bool
outside s@Status{cols, rows, guardPos = (r, c)} = r < 1 || c < 1 || r > rows || c > cols

rotateRight :: Direction -> Direction
rotateRight North = East
rotateRight South = West
rotateRight West = North
rotateRight East = South

findAllPos :: Status -> [Pos]
findAllPos = map guardPos . takeWhile (not . outside) . iterate nextStep

nextStep :: Status -> Status
nextStep s@Status{obstacles, guardDir, guardPos = (r, c)} =
    let nextPos = case guardDir of
            North -> (r - 1, c)
            South -> (r + 1, c)
            West -> (r, c - 1)
            East -> (r, c + 1)
     in if nextPos `elem` obstacles
            then s{guardDir = rotateRight guardDir}
            else s{guardPos = nextPos}

parse :: String -> Status
parse x =
    foldr
        helper
        (Status 0 0 [] (0, 0) North)
        [(r, c, char) | (r, line) <- zip [1 ..] (lines x), (c, char) <- zip [1 ..] line]
  where
    helper (r, c, '#') s@Status{obstacles} = helper (r, c, '.') s{obstacles = (r, c) : obstacles}
    helper (r, c, '^') s = helper (r, c, '.') s{guardPos = (r, c), guardDir = North}
    helper (r, c, 'v') s = helper (r, c, '.') s{guardPos = (r, c), guardDir = South}
    helper (r, c, '<') s = helper (r, c, '.') s{guardPos = (r, c), guardDir = West}
    helper (r, c, '>') s = helper (r, c, '.') s{guardPos = (r, c), guardDir = East}
    helper (r, c, _) s@Status{rows, cols} = s{rows = max r rows, cols = max c cols}

main = interact $ show . length . nub . findAllPos . parse
