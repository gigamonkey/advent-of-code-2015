{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.List
import Data.Numbers.Primes
import Turtle (options, argInt)
import qualified Data.Map.Strict as M

type Tree = M.Map Int [Int]

answer = 665280

puzzle :: Int
puzzle = 2900000

divisors xs p = sort $ nub $ (*) <$> xs <*> [1, p]

start :: Tree
start = M.fromList [(1, [1])]

fill limit (upper, m) [] = (upper, m)
fill limit (upper, m) (p:ps) = if isFull next then next else recur next ps where
    next = update limit (upper, m) p
    recur = fill limit

isFull (upper, m) = sort (M.keys m) == [1..upper]

update limit (upper, m) p = foldl (updatePower limit p) (upper, m) (M.assocs m)

updatePower limit p (upper, m) (k, v) = if sum v >= limit then (upper, m) else (if sum v' >= limit then next else recur next (k', v')) where
    k'     = k * p
    v'     = divisors v p
    upper' = if sum v' > limit then k' else upper
    next   = if k' > upper then (upper, m) else (upper', M.filterWithKey (needed upper' p) (M.insert k' v' m))
    recur  = updatePower limit p

needed upper p k _ = k <= upper && (k * p) <= upper

up = update puzzle

main :: IO ()
main = do
  n <- options "Foo" (argInt  "min" "Minimum number of presents")
  print $ n
