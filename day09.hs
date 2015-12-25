{-# LANGUAGE OverloadedStrings #-}

import Data.List (maximumBy, minimumBy, nub, permutations)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Turtle
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M

data Boolean = True | False

data Problem = Problem [String] (M.Map (String, String) Int)

puzzle = fold (input "puzzles/day09.puzzle") F.list

problem p = Problem cities distances where
    edges     = map (head . match edge) p
    cities    = nub [ city | ((a, b), _) <- edges, city <- [a, b] ]
    distances = foldl (\m (p, d) -> M.insert p d m) M.empty edges
    edge      = do { a <- city; " to "; b <- city; " = "; d <- decimal; return (pair a b, d) }
    city      = some letter

distance ds path = foldl len 0 (pairs path) where
    pairs xs = zip xs (tail xs)
    len d (a, b) = d + fromJust (M.lookup (pair a b) ds)

pair a b = if a < b then (a, b) else (b, a)

solve p = (min paths, max paths) where
    Problem cities distances = problem p
    paths = permutations cities
    len   = distance distances
    max   = len . maximumBy (comparing len)
    min   = len . minimumBy (comparing len)

main = puzzle >>= print . solve
