import Data.List
import System.IO

puzzle = openFile "day3.puzzle" ReadMode >>= hGetContents

visited = nub . scanl next (0, 0) where
    next (x, y) '<' = (x - 1, y)
    next (x, y) '>' = (x + 1, y)
    next (x, y) '^' = (x, y + 1)
    next (x, y) 'v' = (x, y - 1)

everyOther [] = []
everyOther (x : []) = [x]
everyOther (x : _ : xs) = x : everyOther xs

santa = visited . everyOther
robot = visited . everyOther . tail

visited' p = nub $ santa p ++ robot p

main = puzzle >>= print . length . visited'
