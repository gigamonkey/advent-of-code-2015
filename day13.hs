{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.List (nub, permutations, minimumBy, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M

data Problem = Problem [Text] (M.Map (Text, Text) Int) deriving (Show)

puzzle = fold (input "puzzles/day13.puzzle") F.list

problem p = Problem people happiness where
    facts     = map (head . match fact) p
    people    = nub [ p | ((s, o), _) <- facts, p <- [s, o] ]
    happiness = foldl (\m (p, d) -> M.insertWith (+) p d m) M.empty facts

fact = do
  subject <- name
  " would "
  sign <- gainlose
  " "
  amount <- decimal
  " happiness units by sitting next to "
  object <- name
  "."
  return (pair subject object, sign amount)

name = star letter

gainlose = fmap (\x -> if x == "gain" then id else (0 -)) (choice ["gain", "lose"])

pair a b = if a < b then (a, b) else (b, a)

score h seating = foldl sum 0 pairs where
    pairs        = take (length seating) $ zip loop (tail loop)
    loop         = cycle seating
    sum d (a, b) = d + fromJust (M.lookup (pair a b) h)

solve (Problem people happiness) = max paths where
    paths = permutations people
    s     = score happiness
    max   = s . maximumBy (comparing s)

addMe (Problem people happiness) = Problem ("Me" : people) newH where
    newH = foldl (\m p -> M.insert p 0 m) happiness (map (pair "Me") people)

main = puzzle >>= print . solve . addMe . problem
