{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.List
import Data.Numbers.Primes
import Turtle (options, argInt)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type Tree = M.Map Int [Int]

answer = 665280

puzzle = 2900000

takeInclusive p = (\(a, b) -> a ++ [ head b ]) . span p

divisors p xs = sort $ nub $ (*) <$> xs <*> [1, p]

powers p = takeInclusive ((<= puzzle) . sum . snd) . iterate step where
    step = (,) <$> (p *) . fst <*> (divisors p) . snd

start = M.fromList $ powers 2 (1, [1])

addPower m p = M.fromList $ takeInclusive ((<= puzzle) . sum . snd) $ sort next where
    next    = [ x | c <- sort $ M.assocs m, x <- takeWhile ((<= upper) . fst) $ powers p c ]
    upper   = maximum $ M.keys m

addPowers = foldl addPower start

addPowers' m (p:ps) = if m == next then m else addPowers' next ps where
    next = addPower m p

findFirst m = fst $ head $ filter ((>= puzzle) . sum . snd) $ sort $ M.assocs m

bar m (p:ps) = ((p, maximum $ M.keys next, M.size next - M.size m), next) : bar next ps where
    next = addPower m p

foo m p = (length [ x | c <- sort $ M.assocs m, x <- takeWhile ((<= upper) . fst) $ powers p c ])  - (length $ M.elems m) where
    upper = maximum $ M.keys m

main = do
  print $ addPowers (take 10 primes)
  print $ addPowers' start (take 10 primes)
