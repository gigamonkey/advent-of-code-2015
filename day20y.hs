{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<*>))
import Data.List (nub, sort)
import Data.Numbers.Primes (primes)
import Turtle (options, argInt)

type Leaf = (Int, [Int])
type Tree = [Leaf]

theAnswer :: Int
theAnswer = 665280

puzzle :: Int
puzzle = 2900000

takeInclusive :: (a -> Bool) -> [a] -> [a]
takeInclusive p = (\(a, b) -> a ++ take 1 b) . span p

divisors :: Int -> [Int] -> [Int]
divisors p xs = sort $ nub $ (*) <$> xs <*> [1, p]

takeBelow :: Int -> Tree -> Tree
takeBelow lim = takeInclusive ((<= lim) . sum . snd)

powers :: Int -> Int -> Leaf -> Tree
powers lim p = takeBelow lim . iterate step where
    step = (,) <$> (p *) . fst <*> (divisors p) . snd

start :: Int -> Tree
start lim = powers lim 2 (1, [1])

addPower :: Int -> Tree -> Int -> Tree
addPower lim m p = takeBelow lim $ nub $ sort next where
    next  = [ x | c <- sort m, x <- takeWhile ((<= upper) . fst) $ powers lim p c ]
    upper = maximum $ map fst m

addPowers' _ m [] = m
addPowers' lim m (p:ps) = if m == next then m else addPowers' lim next ps where
    next = addPower lim m p

answer :: Tree -> Int
answer = maximum . map fst

--bar :: Int -> Tree -> [Int] -> [((Int, Int, Int), Tree)]
bar lim =  map fst $ takeWhile (\((_, _, _, d), _) -> d > 0) $ foo (start lim) (drop 1 primes) where
    foo m (p:ps) = ((p, last $ map fst next, length $ snd $ last next, length next - length m), next) : foo next ps where
                                                     next = addPower lim m p

main = do
  lim <- options "Advent of Code, Day 20 solver" (argInt  "min" "Minimum number of presents")
  print $ answer $ addPowers' lim (start lim) (drop 1 primes)
