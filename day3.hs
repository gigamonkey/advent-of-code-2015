import Data.List
import System.IO

puzzle = openFile "day3.puzzle" ReadMode >>= hGetContents

visited = nub . scanl next (0, 0) where
    next (x, y) '<' = (x - 1, y)
    next (x, y) '>' = (x + 1, y)
    next (x, y) '^' = (x, y + 1)
    next (x, y) 'v' = (x, y - 1)

main = puzzle >>= print . length . visited
