import Control.Monad
import Data.List
import Data.Matrix
import System.IO

puzzle = liftM lines (openFile "puzzles/day18.puzzle" ReadMode >>= hGetContents)

board p = fromLists $ map (map (== '#')) p

offsets = [ (x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x,y) /= (0, 0) ]

inBounds m (x, y) = 0 < x && x <= nrows m && 0 < y && y <= ncols m

neighbors m (x, y) = map (\p -> inBounds m p && m ! p) [ (x + i, y + j) | (i, j) <- offsets ]

next m p = alive (m ! p) where
    alive True  = count == 2 || count == 3
    alive False = count == 3
    count       = length $ filter (== True) (neighbors m p)

nextMatrix m = fromList (nrows m) (ncols m) [ next m (x, y) | x <- [1 .. nrows m], y <- [1 .. ncols m] ]

on m = sum [ if m ! (x, y) then 1 else 0 | x <- [1 .. nrows m], y <- [1 .. ncols m] ]

display m = intercalate "\n" [ [ if m ! (x, y) then '#' else '.' | y <- [1 .. nrows m] ] | x <- [1 .. ncols m] ]

play m = iterate nextMatrix m

main = puzzle >>= print . on . (\m -> play m !! 100) . board
