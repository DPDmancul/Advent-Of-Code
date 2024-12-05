-- runghc 04-2.hs < 04.in.txt

{-
--- Part Two ---

The Elf looks quizzically at you. Did you misunderstand the assignment?

Looking for the instructions, you flip over the word search to find that this isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed to find two MAS in the shape of an X. One way to achieve that is like this:

M.S
.A.
M.S

Irrelevant characters have again been replaced with . in the above diagram. Within the X, each MAS can be written forwards or backwards.

Here's the same example from before, but this time all of the X-MASes have been kept instead:

.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........

In this example, an X-MAS appears 9 times.

Flip the word search from the instructions back over to the word search side and try again. How many times does an X-MAS appear?
-}


import Data.Array
import Distribution.Simple.Command (OptDescr (BoolOpt))

word = "MAS"

type Mat = Array (Int, Int) Char

readMat :: String -> Mat
readMat x =
    let mat = zip [1 ..] $ map (zip [1 ..]) $ lines x
     in array ((1, 1), (length mat, length $ snd $ head mat)) (mat >>= helper)
  where
    helper (i, xs) = (\(j, x) -> ((i, j), x)) <$> xs

diagonals :: Int -> Mat -> [[String]]
diagonals l m = helper <$> indices m
  where
    b@(_, (r, c)) = bounds m
    helper (i, j) =
        fmap (m !) . filter (inRange b)
            <$> [ [(i + k, j + k) | k <- [0 .. l - 1]] -- principal diagonal
                , [(i + l - k, j + k - 1) | k <- [1 .. l]] -- secondary diagonal
                ]

isMatch :: String -> String -> Bool
isMatch x y = x == y || reverse x == y

main = interact $ show . length . filter (all $ isMatch word) . diagonals (length word) . readMat

