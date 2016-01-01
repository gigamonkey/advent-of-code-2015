{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad
import Data.List (group)

puzzle = liftM lines $ readFile "puzzles/day05.puzzle"

(&&&) a b = (&&) <$> a <*> b

pairs [] = []
pairs [_] = []
pairs (x:y:xs) = [x,y] : pairs (y:xs)

threeVowels       = (> 2) . length . filter (`elem` "aeiou")
atLeastOneDoubled = any ((> 1) . length) . group
noNaughty         = not . any (`elem` ["ab", "cd", "pq", "xy"]) . pairs

nice = threeVowels &&& atLeastOneDoubled &&& noNaughty

-- part 2

nonAdjacentDoubled [] = False
nonAdjacentDoubled [_] = False
nonAdjacentDoubled (x:xs) = (x `elem` tail xs) || nonAdjacentDoubled xs

doubledPair = nonAdjacentDoubled . pairs

separated [_] = False
separated [_, _] = False
separated (x:y:xs) = x == head xs || separated (y:xs)

nice' = doubledPair &&& separated

main = do
  p <- puzzle
  print $ length $ filter nice p
  print $ length $ filter nice' p
