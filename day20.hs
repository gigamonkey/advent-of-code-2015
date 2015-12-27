{-# LANGUAGE OverloadedStrings #-}

import Turtle (options, argInt)
import Control.Applicative
import Data.List

presents = foldr combine zeros [ elf n | n <- [1 .. ] ] where
    elf n = cycle $ n : take (n - 1) zeros
    combine (x:xs) ys = x : zipWith (+) xs ys
    zeros = repeat 0

main = do
  n <- options "Foo" (argInt  "min" "Minimum number of presents")
  print $ find ((>= n) . snd) (zip [1..] presents)
