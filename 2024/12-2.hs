-- runghc 12-2.hs < 12.in.txt

{-
--- Part Two ---

Fortunately, the Elves are trying to order so much fence that they qualify for a bulk discount!

Under the bulk discount, instead of using the perimeter to calculate the price, you need to use the number of sides each region has. Each straight section of fence counts as a side, regardless of how long it is.

Consider this example again:

AAAA
BBCD
BBCC
EEEC

The region containing type A plants has 4 sides, as does each of the regions containing plants of type B, D, and E. However, the more complex region containing the plants of type C has 8 sides!

Using the new method of calculating the per-region price by multiplying the region's area by its number of sides, regions A through E have prices 16, 16, 32, 4, and 12, respectively, for a total price of 80.

The second example above (full of type X and O plants) would have a total price of 436.

Here's a map that includes an E-shaped region full of type E plants:

EEEEE
EXXXX
EEEEE
EXXXX
EEEEE

The E-shaped region has an area of 17 and 12 sides for a price of 204. Including the two regions full of type X plants, this map has a total price of 236.

This map has a total price of 368:

AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA

It includes two regions full of type B plants (each with 4 sides) and a single region full of type A plants (with 4 sides on the outside and 8 more sides on the inside, a total of 12 sides). Be especially careful when counting the fence around regions like the one full of type A plants; in particular, each section of fence has an in-side and an out-side, so the fence does not connect across the middle of the region (where the two B regions touch diagonally). (The Elves would have used the Möbius Fencing Company instead, but their contract terms were too one-sided.)

The larger example from before now has the following updated prices:

    A region of R plants with price 12 * 10 = 120.
    A region of I plants with price 4 * 4 = 16.
    A region of C plants with price 14 * 22 = 308.
    A region of F plants with price 10 * 12 = 120.
    A region of V plants with price 13 * 10 = 130.
    A region of J plants with price 11 * 12 = 132.
    A region of C plants with price 1 * 4 = 4.
    A region of E plants with price 13 * 8 = 104.
    A region of I plants with price 14 * 16 = 224.
    A region of M plants with price 5 * 6 = 30.
    A region of S plants with price 3 * 6 = 18.

Adding these together produces its new total price of 1206.

What is the new total price of fencing all regions on your map?
-}

import Control.Monad.State.Lazy
import Data.Array
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

type Farm = Array (Int, Int) Char

justIf :: (a -> Bool) -> a -> Maybe a
justIf f x = if f x then Just x else Nothing

price :: Farm -> Int
price farm = sum $ evalState (traverse helper (assocs farm)) Set.empty
  where
    helper :: ((Int, Int), Char) -> State (Set (Int, Int)) Int
    helper (i, x) = do
        plotBorders <- visit i
        let area = length plotBorders
        let borders = sum plotBorders
        return $ area * borders
      where
        visit :: (Int, Int) -> State (Set (Int, Int)) [Int]
        visit j@(r, c) = do
            visited <- get
            if j `Set.member` visited
                then return []
                else do
                    put $ Set.insert j visited
                    -- # borders = # corners
                    let borders = length $ filter (`elem` cornerPatterns) nearbyCorners
                    nearbyBorders <- traverse visit $ filter ((== x) . (farm !)) $ catMaybes nearby
                    return $ borders : concat nearbyBorders
          where
            nearby = map (justIf $ inRange $ bounds farm) [(r, c - 1), (r, c + 1), (r - 1, c), (r + 1, c)]
            nearbyCorners = [[(Just x ==) $ (farm !) <$> justIf (inRange $ bounds farm) (r + i, c + j) | (i, j) <- delta] | delta <- cornerDeltas]
            cornerDeltas =
                [ [(0, 1), (1, 1), (1, 0)]
                , [(1, 0), (1, -1), (0, -1)]
                , [(0, -1), (-1, -1), (-1, 0)]
                , [(-1, 0), (-1, 1), (0, 1)]
                ]
            cornerPatterns =
                [ [False, False, False]
                , [False, True, False]
                , [True, False, True]
                ]

main = interact $ show . price . parse
  where
    parse x =
        let rows = filter (not . null) $ lines x
         in listArray ((1, 1), (length rows, length $ head rows)) (filter (/= '\n') x)
