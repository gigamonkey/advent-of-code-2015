{-# LANGUAGE OverloadedStrings #-}

import Data.List (find)
import Data.Maybe (fromJust)
import Turtle (options, argInt)

presents :: [Int]
presents = foldr combine zeros [ elf n | n <- [1 .. ] ] where
    elf n = cycle $ n : take (n - 1) zeros
    combine (x:xs) ys = x : zipWith (+) xs ys
    zeros = repeat 0

main :: IO ()
main = do
  n <- options "Advent of Code, day 20 solver" (argInt  "min" "Minimum number of presents")
  print $ fst $ fromJust $ find ((>= n) . snd) (zip [1..] presents)
