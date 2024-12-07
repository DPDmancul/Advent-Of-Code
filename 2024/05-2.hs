-- runghc 05-2.hs < 05.in.txt

{-
--- Part Two ---

While the Elves get to work printing the correctly-ordered updates, you have a little time to fix the rest of them.

For each of the incorrectly-ordered updates, use the page ordering rules to put the page numbers in the right order. For the above example, here are the three incorrectly-ordered updates and their correct orderings:

    75,97,47,61,53 becomes 97,75,47,61,53.
    61,13,29 becomes 61,29,13.
    97,13,75,29,47 becomes 97,75,47,29,13.

After taking only the incorrectly-ordered updates and ordering them correctly, their middle page numbers are 47, 29, and 47. Adding these together produces 123.

Find the updates which are not in the correct order. What do you get if you add up the middle page numbers after correctly ordering just those updates?
-}

import Data.Bifunctor
import Data.List

type Rule = (Int, Int)
type Update = [Int]

parseRule :: String -> Rule
parseRule = bimap read (read . tail) . span (/= '|')

parseUpdate :: String -> Update
parseUpdate x = read $ '[' : x ++ "]"

order :: [Rule] -> Update -> (Bool, Update)
order rules = foldr helper1 (True, [])
 where
  helper1 x (wasOrdered, xs) =
    let (orderChanged, xs') = foldr helper2 (False, []) xs
     in if orderChanged then (False, xs') else (wasOrdered, x : xs)
   where
    helper2 y (False, ys) | (y, x) `elem` rules = (True, y : x : ys)
    helper2 y (orderChanged, ys) = (orderChanged, y : ys)

takeMiddle :: [Int] -> Int
takeMiddle xs = xs !! (length xs `div` 2)

main = interact $ show . helper
 where
  helper x =
    let (rules, _ : updates) = span (/= []) $ lines x
     in sum $ map (takeMiddle . snd) $ filter (not . fst) $ map (order (map parseRule rules) . parseUpdate) updates
