-- runghc 04-1.hs < 04.in.txt

{-
--- Day 4: Ceres Search ---

"Looks like the Chief's not here. Next!" One of The Historians pulls out a device and pushes the only button on it. After a brief flash, you recognize the interior of the Ceres monitoring station!

As the search for the Chief continues, a small Elf who lives on the station tugs on your shirt; she'd like to know if you could help her with her word search (your puzzle input). She only has to find one word: XMAS.

This word search allows words to be horizontal, vertical, diagonal, written backwards, or even overlapping other words. It's a little unusual, though, as you don't merely need to find one instance of XMAS - you need to find all of them. Here are a few ways XMAS might appear, where irrelevant characters have been replaced with .:

..X...
.SAMX.
.A..A.
XMAS.S
.X....

The actual word search will be full of letters instead. For example:

MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX

In this word search, XMAS occurs a total of 18 times; here's the same word search again, but where letters not involved in any XMAS have been replaced with .:

....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX

Take a look at the little Elf's word search. How many times does XMAS appear?
-}

import Data.Array

word = "XMAS"

type Mat = Array (Int, Int) Char

readMat :: String -> Mat
readMat x =
    let mat = zip [1 ..] $ map (zip [1 ..]) $ lines x
     in array ((1, 1), (length mat, length $ snd $ head mat)) (mat >>= helper)
  where
    helper (i, xs) = (\(j, x) -> ((i, j), x)) <$> xs

subStrings :: Int -> Mat -> [String]
subStrings l m = indices m >>= helper
  where
    b@(_, (r, c)) = bounds m
    helper (i, j) =
        fmap (m !) . filter (inRange b)
            <$> [ [(i + k, j) | k <- [0 .. l - 1]] -- vertical
                , [(i, j + k) | k <- [0 .. l - 1]] -- horizontal
                , [(i + k, j + k) | k <- [0 .. l - 1]] -- principal diagonal
                , [(i + l - k, j + k - 1) | k <- [1 .. l]] -- secondary diagonal
                ]

isMatch :: String -> String -> Bool
isMatch x y = x == y || reverse x == y

main = interact $ show . length . filter (isMatch word) . subStrings (length word) . readMat
