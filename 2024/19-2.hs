-- runghc 19-2.hs < 19.in.txt

{-
--- Part Two ---

The staff don't really like some of the towel arrangements you came up with. To avoid an endless cycle of towel rearrangement, maybe you should just give them every possible option.

Here are all of the different ways the above example's designs can be made:

brwrr can be made in two different ways: b, r, wr, r or br, wr, r.

bggr can only be made with b, g, g, and r.

gbbr can be made 4 different ways:

- g, b, b, r
- g, b, br
- gb, b, r
- gb, br

rrbgbr can be made 6 different ways:

- r, r, b, g, b, r
- r, r, b, g, br
- r, r, b, gb, r
- r, rb, g, b, r
- r, rb, g, br
- r, rb, gb, r

bwurrg can only be made with bwu, r, r, and g.

brgr can be made in two different ways: b, r, g, r or br, g, r.

ubwu and bbrgwb are still impossible.

Adding up all of the ways the towels in this example could be arranged into the desired designs yields 16 (2 + 1 + 4 + 6 + 1 + 2).

They'll let you into the onsen as soon as you have the list. What do you get if you add up the number of different ways you could make each design?
-}

import Control.Monad.State
import Data.Coerce
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List
import Data.Maybe

data Color = White | Blue | Black | Red | Green
    deriving (Eq, Ord, Enum)

colorToChar :: Color -> Char
colorToChar White = 'w'
colorToChar Blue = 'u'
colorToChar Black = 'b'
colorToChar Red = 'r'
colorToChar Green = 'g'

instance Show Color where
    show = return . colorToChar
    showList xs s = map colorToChar xs ++ s

charToColor :: Char -> Color
charToColor 'w' = White
charToColor 'u' = Blue
charToColor 'b' = Black
charToColor 'r' = Red
charToColor 'g' = Green
charToColor x = error $ "Unkwnon color " ++ [x]

instance Read Color where
    readsPrec _ (x : xs) = return (charToColor x, xs)
    readList x =
        let
            (colors, xs) = span (`elem` "wubrg") x
         in
            return (map charToColor colors, xs)

type Pattern = [Color]
type Design = [Color]

hashDesign :: Design -> Int
hashDesign = foldr (\x acc -> acc * 6 + fromEnum x + 1) 0

newtype DesignMap a = DesignMap (IntMap a)

designLookup :: Design -> DesignMap a -> Maybe a
designLookup k = IntMap.lookup (hashDesign k) . coerce

designInsert :: Design -> a -> DesignMap a -> DesignMap a
designInsert k v = coerce $ IntMap.insert (hashDesign k) v

nothingIfNull :: (Traversable t) => t a -> Maybe (t a)
nothingIfNull x | null x = Nothing
nothingIfNull x = Just x

countArrangments :: [Pattern] -> Design -> Int
countArrangments patterns = fromMaybe 0 . flip evalState (DesignMap IntMap.empty) . memoize
  where
    memoize [] = return $ Just 1
    memoize design = do
        cache <- get
        case designLookup design cache of
            Just x -> return x
            Nothing -> do
                x <- fmap sum . nothingIfNull . catMaybes <$> mapM match patterns
                modify $ designInsert design x
                return x
      where
        match pattern = maybe (return Nothing) memoize $ stripPrefix pattern design


main = interact $ show . sum . uncurry (map . countArrangments) . parse
  where
    parse :: String -> ([Pattern], [Design])
    parse x =
        let
            patterns : designs = filter (not . null) $ lines x
         in
            (read $ '[' : filter (/= ' ') patterns ++ "]", map read designs)
