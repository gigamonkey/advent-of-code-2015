{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Applicative
import Control.Monad
import Data.List (group)

puzzle = liftM lines (readFile "puzzles/day05.puzzle")

foo fn fns a = foldl fn (head fns a) (map (\f -> f a) (tail fns))

(&&&) a b = (&&) <$> a <*> b

pairs [] = []
pairs [x] = []
pairs (x:y:xs) = [x,y] : pairs (y:xs)

threeVowels       = (> 2) . length . filter (`elem` "aeiou")
atLeastOneDoubled = any ((> 1) . length) . group
noNaughty         = not . any (`elem` ["ab", "cd", "pq", "xy"]) . pairs

nice = threeVowels &&& atLeastOneDoubled &&& noNaughty

-- part 2

nonAdjacentDoubled [] = False
nonAdjacentDoubled [x] = False
nonAdjacentDoubled (x:xs) = (x `elem` tail xs) || nonAdjacentDoubled xs

doubledPair = nonAdjacentDoubled . pairs

separated [_] = False
separated [_, _] = False
separated (x:y:xs) = x == head xs || separated (y:xs)

nice' = doubledPair &&& separated

main = puzzle >>= print . length . filter nice'
