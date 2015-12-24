{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe
import Data.Ord
import Turtle
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M

data Problem = Problem [String] (M.Map (String, String) Int)

puzzle = fold (input "puzzles/day9.puzzle") F.list

parse p = Problem cities distances where
    edges     = map (head . (match edge)) p
    cities    = nub [ city | ((a, b), _) <- edges, city <- [a, b] ]
    distances = foldl (\m (p, d) -> M.insert p d m) M.empty edges
    city      = some letter
    edge      = do { a <- city; " to "; b <- city; " = "; d <- decimal; return (pair a b, d) }

pair a b = (min a b, max a b)

pathLength ds path = foldl len 0 (pairs path) where
    pairs xs = zip xs (tail xs)
    len d (a, b) = d + (fromJust $ M.lookup (pair a b) ds)

main = do
  p <- puzzle
  let (Problem cities distances) = parse p
  let shortest = minimumBy (comparing (pathLength distances)) (permutations cities)
  print $ pathLength distances shortest
