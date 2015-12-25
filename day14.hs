{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.List (nub, permutations, minimumBy, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Control.Foldl as F

data Reindeer = Reindeer Text Int Int Int deriving (Show)

puzzle = fold (input "puzzles/day14.puzzle") F.list

fact = do
  name <- (star letter)
  " can fly "
  speed <- decimal
  " km/s for "
  flyTime <- decimal
  " seconds, but then must rest for "
  restTime <- decimal
  " seconds."
  return (Reindeer name speed flyTime restTime)

distance n (Reindeer name speed flyTime restTime) = (name, d) where
    rounds     = n `div` (flyTime + restTime)
    left       = n - (rounds * (flyTime + restTime))
    lastFlight = min flyTime left
    d          = ((rounds * flyTime) + lastFlight) * speed

main = puzzle >>= print . maximumBy (comparing snd) . map (distance 2503 . head . match fact)
