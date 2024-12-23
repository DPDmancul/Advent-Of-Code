-- runghc 17-2.hs < 17.in.txt

{-
--- Part Two ---

Digging deeper in the device's manual, you discover the problem: this program is supposed to output another copy of the program! Unfortunately, the value in register A seems to have been corrupted. You'll need to find a new value to which you can initialize register A so that the program's output instructions produce an exact copy of the program itself.

For example:

Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0

This program outputs a copy of itself if register A is instead initialized to 117440. (The original initial value of register A, 2024, is ignored.)

What is the lowest positive initial value for register A that causes the program to output a copy of itself?
-}

{-
0: 2 4 → B = A mod 8
1: 1 2 → B = B xor 2 = (A mod 8) xor 2
2: 7 5 → C = A / 2 ^ B = A / 2 ^ ((A mod 8) xor 2)
3: 4 7 → B = B xor C = (A mod 8) xor 2 xor (A / 2 ^ ((A mod 8) xor 2))
4: 1 3 → B = B xor 3 = (A mod 8) xor 2 xor (A / 2 ^ ((A mod 8) xor 2)) xor 3
5: 5 5 → print B mod 8 = print ((A mod 8) xor 2 xor (A / 2 ^ ((A mod 8) xor 2)) xor 5) mod 8
6: 0 3 → A = A / 8
7: 3 0 → A == 0 ⇒ goto 0

so the whole program is

while (A > 0)
{
    print ((A mod 8) xor 2 xor (A / 2 ^ ((A mod 8) xor 2)) xor 5) mod 8
    A = A / 8
}
-}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.Bits
import Data.Maybe

type Parser = StateT String Maybe

type Program = [Int]

findRegA :: Program -> Int
findRegA = minimum . helper
  where
    helper [] = [0]
    helper (x : xs) = filter isValid $ (+) <$> [0 .. 7] <*> map (* 8) (helper xs)
        where
        isValid a =
            let axor2 = (a `mod` 8) `xor` 2
             in (axor2 `xor` (a `div` (2 ^ axor2)) `xor` 3) `mod` 8 == x

main = interact $ show . findRegA . fromJust . evalStateT parser
  where
    parser = (,) <$> parseRegisters *> some parseLF *> parseProgram
    parseRegisters = parseRegister "A" *> parseRegister "B" *> parseRegister "C"
    parseRegister x = parseString ("Register " ++ x ++ ": ") *> parseInt <* parseLF
    parseProgram :: Parser Program = parseString "Program: " *> parseList
    parseList = (:) <$> parseInt <*> some (parseChar ',' *> parseInt)
    parseInt :: Parser Int = read <$> some (parseIf (`elem` ['0' .. '9']))
    parseLF = parseChar '\n'
    parseString = traverse parseChar
    parseChar = parseIf . (==)
    parseIf p = do
        x : xs <- get
        guard $ p x
        put xs
        return x
