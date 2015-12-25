{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.List (nub, permutations, minimumBy, maximumBy)
import Data.Ord (comparing)
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M

data Ingredient = Ingredient
    { name :: Text
    , capacity :: Int
    , durability :: Int
    , flavor :: Int
    , texture :: Int
    , calories :: Int
    } deriving (Show)

puzzle = fold (input "puzzles/day15.puzzle") F.list

ingredient = do
  name <- star letter
  ": "
  c    <- labeled "capacity"
  d    <- labeled "durability"
  f    <- labeled "flavor"
  t    <- labeled "texture"
  cal  <- labeled "calories"
  return (Ingredient name c d f t cal)

labeled label = do { label; " "; n <- signed decimal; option ", "; return n }

splits _ 0 = []
splits n 1 = [[n]]
splits n p = [ i : rest | i <- [0 .. n], rest <- splits (n - i) (p - 1) ]

fns = [ capacity, durability, flavor, texture ]

score ingredients split = product (map (max 0 . combined) fns) where
    combined f = foldl (\tot (amount, ingredient) -> tot + (f ingredient * amount)) 0 (zip split ingredients)

cookieCalories ingredients split = foldl (\tot (amount, ingredient) -> tot + (calories ingredient * amount)) 0 (zip split ingredients)

main = do
  p <- puzzle
  let ingredients = map (head . match ingredient) p
  print $ maximum $ map (score ingredients) (filter ((== 500) . cookieCalories ingredients) (splits 100 (length ingredients)))
