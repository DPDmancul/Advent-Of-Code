-- runghc 09-2.hs < 09.in.txt

{-
--- Part Two ---

Upon completion, two things immediately become clear. First, the disk definitely has a lot more contiguous free space, just like the amphipod hoped. Second, the computer is running much more slowly! Maybe introducing all of that file system fragmentation was a bad idea?

The eager amphipod already has a new plan: rather than move individual blocks, he'd like to try compacting the files on his disk by moving whole files instead.

This time, attempt to move whole files to the leftmost span of free space blocks that could fit the file. Attempt to move each file exactly once in order of decreasing file ID number starting with the file with the highest file ID number. If there is no span of free space to the left of a file that is large enough to fit the file, the file does not move.

The first example from above now proceeds differently:

00...111...2...333.44.5555.6666.777.888899
0099.111...2...333.44.5555.6666.777.8888..
0099.1117772...333.44.5555.6666.....8888..
0099.111777244.333....5555.6666.....8888..
00992111777.44.333....5555.6666.....8888..

The process of updating the filesystem checksum is the same; now, this example's checksum would be 2858.

Start over, now compacting the amphipod's hard drive using this new method instead. What is the resulting filesystem checksum?
-}

import Data.Char (digitToInt, isDigit)
import Data.Foldable
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq

data Block
    = Space {size :: Int}
    | File {size :: Int, fileId :: Int}

defrag :: [Block] -> [Block]
defrag = toList . helper . Seq.fromList
  where
    helper Empty = Empty
    helper (xs :|> s@(Space _)) = helper xs :|> s
    helper (xs :|> f) = maybe (helper xs :|> f) helper $ putFile f xs
    putFile _ Empty = Nothing
    putFile f@File{size} (Space n :<| xs) | n >= size = Just $ (f :<| Space (n - size) :<| xs) :|> Space size
    putFile f (x :<| xs) = (x :<|) <$> putFile f xs

checksum :: [Block] -> Int
checksum = sum . zipWith (*) [0 ..] . helper
  where
    helper [] = []
    helper (Space n : xs) = replicate n 0 ++ helper xs
    helper (File n x : xs) = replicate n x ++ helper xs

main = interact $ show . checksum . defrag . parse
  where
    parse = readFile 0 . map digitToInt . filter isDigit
    readFile _ [] = []
    readFile x (n : ns) = File n x : readSpace x ns
    readSpace _ [] = []
    readSpace x (n : ns) = Space n : readFile (x + 1) ns
