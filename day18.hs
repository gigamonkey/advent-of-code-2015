import Control.Monad
import Data.Matrix

puzzle = liftM lines $ readFile "puzzles/day18.puzzle"

board p = fromLists $ map (map (== '#')) p

countOn = length . filter id

neighbors m (x, y) = map (\p -> inBounds p && m ! p) [ (x + i, y + j) | (i, j) <- offsets ] where
    inBounds (x, y) = 0 < x && x <= nrows m && 0 < y && y <= ncols m
    offsets = [ (x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0) ]

nextCell m p = alive (m ! p) where
    alive x = count == 3 || (x && count == 2)
    count   = countOn (neighbors m p)

stickyCorners m p f = isCorner p || f m p where
    isCorner = flip elem [ (x, y) | x <- [1, nrows m], y <- [1, ncols m]]

nextState m = fromList (nrows m) (ncols m) [ nextCell m (x, y) | x <- [1 .. nrows m], y <- [1 .. ncols m] ]

nextState' m = fromList (nrows m) (ncols m) [ stickyCorners m (x, y) nextCell | x <- [1 .. nrows m], y <- [1 .. ncols m] ]


stickCorners m = fromList (nrows m) (ncols m) [ stickyCorners m (x, y) (!) | x <- [1 .. nrows m], y <- [1 .. ncols m] ]

play = iterate nextState
play' = iterate nextState'

on m = countOn [ m ! (x, y) | x <- [1 .. nrows m], y <- [1 .. ncols m] ]

part1 = on . (!! 100) . play . board
part2 = on . (!! 100) . play' . stickCorners . board

main = do
  p <- puzzle
  print $ part1 p
  print $ part2 p
