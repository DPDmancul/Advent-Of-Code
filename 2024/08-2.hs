-- runghc 08-2.hs < 08.in.txt

{-
--- Part Two ---

Watching over your shoulder as you work, one of The Historians asks if you took the effects of resonant harmonics into your calculations.

Whoops!

After updating your model, it turns out that an antinode occurs at any grid position exactly in line with at least two antennas of the same frequency, regardless of distance. This means that some of the new antinodes will occur at the position of each antenna (unless that antenna is the only one of its frequency).

So, these three T-frequency antennas now create many antinodes:

T....#....
...T......
.T....#...
.........#
..#.......
..........
...#......
..........
....#.....
..........

In fact, the three T-frequency antennas are all exactly in line with two antennas, so they are all also antinodes! This brings the total number of antinodes in the above example to 9.

The original example now has 34 antinodes, including the antinodes that appear on every antenna:

##....#....#
.#.#....0...
..#.#0....#.
..##...0....
....0....#..
.#...#A....#
...#..#.....
#....#.#....
..#.....A...
....#....A..
.#........#.
...#......##

Calculate the impact of the signal using this updated model. How many unique locations within the bounds of the map contain an antinode?
-}

import Data.List

type Pos = (Int, Int)
data Antenna = Antenna Char Pos

antinodes :: Int -> Int -> [Antenna] -> [Pos]
antinodes rows cols = concatMap (concatMap pairAntinodes . pairs . map (\(Antenna _ pos) -> pos)) . groupBy eqFreq . sortBy ordFreq
 where
  ordFreq (Antenna f1 _) (Antenna f2 _) = compare f1 f2
  eqFreq (Antenna f1 _) (Antenna f2 _) = f1 == f2
  pairs [] = []
  pairs (x : xs) = [(x, y) | y <- xs] ++ pairs xs
  pairAntinodes ((r1, c1), (r2, c2)) =
    concat [[(r1 + i * stepRow, c1 + i * stepCol), (r1 - i * stepRow, c1 - i * stepCol)] | i <- [0 .. max rows cols]]
   where
    dr = r1 - r2
    dc = c1 - c2
    unit = gcd dr dc
    stepRow = dr `div` unit
    stepCol = dc `div` unit

main = interact $ show . length . nub . helper
 where
  helper file = filter inCity $ antinodes rows cols antennas
   where
    city = [Antenna f (r, c) | (r, line) <- zip [1 ..] (lines file), (c, f) <- zip [1 ..] line]
    (rows, cols, antennas) = foldr visit (0, 0, []) city
    inCity (r, c) = 0 < r && r <= rows && 0 < c && c <= cols
    visit x@(Antenna f (r, c)) (rows, cols, antennas) =
      (max rows r, max cols c, if f == '.' then antennas else x : antennas)
