-- runghc 11-2.hs < 11.in.txt

{-
--- Part Two ---

The Historians sure are taking a long time. To be fair, the infinite corridors are very large.

How many stones would you have after blinking a total of 75 times?
-}

import Data.Function
import Data.List

data Group = Group {qty :: Int, val :: Int}

blink :: [Group] -> [Group]
blink = group' . helper
  where
  helper [] = []
  helper (Group n 0 : xs) = Group n 1 : blink xs
  helper (Group n x : xs)
    | even numOfDigits =
        let
          (l, r) = splitAt (numOfDigits `div` 2) digits
         in
          Group n (read l) : Group n (read r) : helper xs
    | otherwise = Group n (2024 * x) : helper xs
   where
    digits = show x
    numOfDigits = length digits

group' :: [Group] -> [Group]
group' = helper . sortBy (on compare val)
  where
    helper [] = []
    helper (Group n x : Group m y : xs) | x == y = helper $ Group (n + m) x : xs
    helper (x : xs) = x : helper xs

main = interact $ show . sum . map qty . nTimes 75 blink . group' . map (Group 1 . read) . words
 where
  nTimes 0 _ = id
  nTimes n f = f . nTimes (n - 1) f

