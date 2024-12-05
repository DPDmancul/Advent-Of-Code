-- runghc 03-2.hs < 03.in.txt

{-
--- Part Two ---

As you scan through the corrupted memory, you notice that some of the conditional statements are also still intact. If you handle some of the uncorrupted conditional statements in the program, you might be able to get an even more accurate result.

There are two new instructions you'll need to handle:

    The do() instruction enables future mul instructions.
    The don't() instruction disables future mul instructions.

Only the most recent do() or don't() instruction applies. At the beginning of the program, mul instructions are enabled.

For example:

xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))

This corrupted memory is similar to the example from before, but this time the mul(5,5) and mul(11,8) instructions are disabled because there is a don't() instruction before them. The other mul instructions function normally, including the one at the end that gets re-enabled by a do() instruction.

This time, the sum of the results is 48 (2*4 + 8*5).

Handle the new instructions; what do you get if you add up all of the results of just the enabled multiplications?
-}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.Bits (Bits (xor))
import Data.Maybe

parse :: String -> [(Int, Int)]
parse = catMaybes . fromJust . evalStateT (evalStateT (many parse) True)
  where
    parse :: StateT Bool Parser (Maybe (Int, Int))
    parse = parseMul <|> parseDo <|> parseDont <|> lift parseAnyChar *> parse
    parseDo = StateT $ const $ (Nothing, True) <$ parseString "do()"
    parseDont = StateT $ const $ (Nothing, False) <$ parseString "don't()"
    parseMul = StateT $ \x -> toMaybe x <$> (parseString "mul(" *> parseTuple <* parseString ")")
    toMaybe True x = (Just x, True)
    toMaybe _ _ = (Nothing, False)
    parseTuple = (\x _ y -> (read x, read y)) <$> parseDigits <*> parseChar ',' <*> parseDigits
    parseDigits = do
        res <- some parseDigit
        guard $ length res <= 3
        return res
    parseDigit = parseCharIf (`elem` "0123456789")

main = interact $ show . sum . map (uncurry (*)) . parse

type Parser = StateT String Maybe

parseAnyChar :: Parser Char
parseAnyChar = StateT helper
  where
    helper (x : xs) = Just (x, xs)
    helper _ = Nothing

parseCharIf :: (Char -> Bool) -> Parser Char
parseCharIf p = do
    x <- parseAnyChar
    guard $ p x
    return x

parseChar :: Char -> Parser Char
parseChar x = parseCharIf (x ==)

parseString :: String -> Parser String
parseString = traverse parseChar
