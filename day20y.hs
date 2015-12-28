{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<*>))
import Data.List (nub, sort)
import Data.Numbers.Primes (primes)

answer = 665280

puzzle = 2900000

takeInclusive p = (\(a, b) -> a ++ [ head b ]) . span p

divisors p xs = sort $ nub $ (*) <$> xs <*> [1, p]

takeBelow = takeInclusive $ (<= puzzle) . sum . snd

powers p = takeBelow . iterate step where
    step = (,) <$> (p *) . fst <*> (divisors p) . snd

start = powers 2 (1, [1])

addPower m p = takeBelow $ nub $ sort next where
    next  = [ x | c <- sort m, x <- takeWhile ((<= upper) . fst) $ powers p c ]
    upper = maximum $ map fst m

addPowers = foldl addPower start

addPowers' m [] = m
addPowers' m (p:ps) = if m == next then m else addPowers' next ps where
    next = addPower m p

findFirst m = fst $ head $ filter ((>= puzzle) . sum . snd) $ sort m

bar m (p:ps) = ((p, maximum $ map fst next, length next - length m), next) : bar next ps where
    next = addPower m p

main = do
  print $ addPowers (take 10 primes)
  print $ addPowers' start (take 10 primes)
