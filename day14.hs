{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.List (nub, permutations, minimumBy, maximumBy)
import Data.Ord (comparing)
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M

data Reindeer = Reindeer Text Int Int Int deriving (Show)

puzzle = fold (input "puzzles/day14.puzzle") F.list

duration = 2503

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

distance n (Reindeer name speed flyTime restTime) = (name, dist) where
    rounds     = n `div` (flyTime + restTime)
    left       = n - (rounds * (flyTime + restTime))
    lastFlight = min flyTime left
    dist       = ((rounds * flyTime) + lastFlight) * speed

reindeer p = map (head . match fact) p

race reindeer = [ map (distance s) reindeer | s <- [1..duration] ]

leaders = map ahead where
    ahead xs = map fst $ filter ((== m xs) . snd) xs
    m xs = snd (maximumBy (comparing snd) xs)

counts = foldl (\m x -> M.insertWith (+) x 1 m) M.empty

score = maximum . M.elems . counts . concat

main = puzzle >>= print . score . leaders . race . reindeer
