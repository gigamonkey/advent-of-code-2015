import Control.Applicative ((<|>))
import Control.Monad
import Data.List (isPrefixOf, null)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import System.IO

type Lights = S.Set (Int, Int)

data Op = On Lights | Off Lights | Toggle Lights deriving (Show)

puzzle = liftM lines (openFile "puzzles/day6.puzzle" ReadMode >>= hGetContents)

splitOn x [] = []
splitOn x xs = head : splitOn x thetail where
    (head, rest) = span (/= x) xs
    thetail = if null rest then [] else tail rest

parse x = fromMaybe (error "No parse") (tryParse On "turn on " x <|> tryParse Off "turn off " x <|> tryParse Toggle "toggle " x)

tryParse f p x = if p `isPrefixOf` x then Just $ f $ parseRest $ drop (length p) x else Nothing

parseRest x = S.fromList [(x, y) | x <- [x1..x2], y <- [y1..y2]] where
    [a, _, b] = splitOn ' ' x
    [x1, y1] = map read (splitOn ',' a)
    [x2, y2] = map read (splitOn ',' b)

turnOn = S.union

turnOff = S.difference

toggle lights toggle = turnOn (turnOff lights off) on where
    off = lights `S.intersection` toggle
    on  = toggle `S.difference` lights

run = foldl eval S.empty where
    eval l (On s)     = turnOn l s
    eval l (Off s)    = turnOff l s
    eval l (Toggle s) = toggle l s

run' = foldl eval M.empty where
    eval m (On s)     = foldl (\m' k -> M.insertWith (+) k 1 m') m (S.elems s)
    eval m (Off s)    = foldl (\m' k -> M.insertWith dim k 0 m') m (S.elems s)
    eval m (Toggle s) = foldl (\m' k -> M.insertWith (+) k 2 m') m (S.elems s)
    dim :: Int -> Int -> Int
    dim _ old = max 0 (old - 1)

part1 = puzzle >>= print . (400410 ==) . S.size . run . map parse

part2 = puzzle >>= print . (M.foldl (+) 0) . run' . map parse

main = part2
