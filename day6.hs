import Control.Applicative
import Control.Monad
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Set (difference, intersection, union, fromList, Set, empty, size)
import System.IO

-- part 1 answer: 400410

data Op = On (Set (Int, Int)) | Off (Set (Int, Int)) | Toggle (Set (Int, Int)) deriving (Show)

puzzle = liftM lines (openFile "puzzles/day6.puzzle" ReadMode >>= hGetContents)

splitOn x [] = []
splitOn x xs = head : splitOn x thetail where
    (head, rest) = span (/= x) xs
    thetail = if null rest then [] else tail rest

parse x = fromMaybe (error "No parse") (tryParse On "turn on " x <|> tryParse Off "turn off " x <|> tryParse Toggle "toggle " x)

tryParse f p x = if p `isPrefixOf` x then Just $ f $ parseRest $ drop (length p) x else Nothing

parseRest x = fromList [(x, y) | x <- [x1..x2], y <- [y1..y2]] where
    [a, _, b] = splitOn ' ' x
    [x1, y1] = map read (splitOn ',' a)
    [x2, y2] = map read (splitOn ',' b)

turnOn, turnOff, toggle :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)

turnOn lights on = lights `union` on

turnOff lights off = lights `difference` off

toggle lights toggle = turnOn (turnOff lights off) on where
    off = lights `intersection` toggle
    on  = toggle `difference` lights

run = foldl foo Data.Set.empty where
    foo l (On s)     = turnOn l s
    foo l (Off s)    = turnOff l s
    foo l (Toggle s) = toggle l s

main = puzzle >>= print . (400410 ==) . size . run . map parse
