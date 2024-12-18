-- runghc 14-2.hs < 14.in.txt

{-
--- Part Two ---

During the bathroom break, someone notices that these robots seem awfully similar to ones built and used at the North Pole. If they're the same type of robots, they should have a hard-coded Easter egg: very rarely, most of the robots should arrange themselves into a picture of a Christmas tree.

What is the fewest number of seconds that must elapse for the robots to display the Easter egg?
-}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.Function
import Data.List
import Data.Maybe

type Parser = StateT String Maybe

type Pos = (Int, Int)
type Speed = (Int, Int)

width = 101
height = 103

step :: Speed -> Pos -> Pos
step (vx, vy) (x, y) = ((x + vx) `mod` width, (y + vy) `mod` height)

isTree :: [Pos] -> Bool
isTree = any isRow . groupBy (on (==) fst) . sortBy (on compare fst)
 where
  isRow xs | length xs < 20 = False
  isRow xs =
    let ys = nub $ sort $ map snd xs
        deltas = zipWith (-) (drop 1 ys) ys
     in any ((> 20) . length) $ filter ((== 1) . head) $ group deltas

main = interact $ show . findIndex isTree . take 10000 . transpose . map (\(pos, speed) -> iterate (step speed) pos) . parse
 where
  parse = fromJust . evalStateT (many parseLine)
  parseLine = (,) <$> parsePos <* parseChar ' ' <*> parseSpeed <* parseLF
  parsePos = (,) <$> (parseString "p=" *> parseInt) <* parseChar ',' <*> parseInt
  parseSpeed = (,) <$> (parseString "v=" *> parseInt) <* parseChar ',' <*> parseInt
  parseInt :: Parser Int = read <$> some (parseIf (`elem` '-' : ['0' .. '9']))
  parseLF = parseChar '\n'
  parseString = traverse parseChar
  parseChar = parseIf . (==)
  parseIf p = do
    x : xs <- get
    guard $ p x
    put xs
    return x
