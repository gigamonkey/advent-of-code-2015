{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord
import Turtle
import Turtle (options, argInt)
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M

type Factors = [(Int, Int)]

puzzle :: Int
puzzle = 29000000

--lim = puzzle `div` 10

bestish, better, evenbetter :: Factors
bestish    = [(2,5),(3,1),(5,1),(7,1),(241,1)]
better     = [(2,3),(3,3),(5,1),(7,1),(107,1)]
evenbetter = [(2,2),(3,5),(5,1),(7,1),(23,1)]

allPrimes :: [Int]
allPrimes = 2 : filter isPrime [3 .. ] where
    isPrime n = not (any ((== 0) . (n `mod`)) $ takeWhile ((<= n) . (^ 2)) allPrimes)

start :: Int -> Factors
start lim = [(2,floor $ logBase 2 (fromIntegral lim))]

-- Number of presents delivered to house with the given prime factorization.
presents :: Factors -> Int
presents = (+ 1) . sum . map house . divisors

-- House number for given prime factorization.
house :: Factors -> Int
house = product . map (\(b, e) -> b ^ e)

-- Does the given house get enough presents?
good :: Int -> Factors -> Bool
good lim = (>= lim) . presents

-- All the divisors for the given prime factorization
divisors :: Factors -> [Factors]
divisors = foldl step [] . map powers where
    step a b = sortBy (comparing house) $ nub (a ++ b ++ ((++) <$> a <*> b))
    powers (n, c) = [ [(n, i)] | i <- [1 .. c] ]

fixpoint f x = if f x == x then x else fixpoint f (f x)

search lim = fixpoint step (start lim) where
    primes = tail (takeWhile (<= lim) allPrimes)
    step xs = fixpoint (down lim primes) (fixpoint (across lim primes)  xs)

across lim primes fs = step fs primes where
    step xs (p:ps) = fromMaybe xs ((try' lim p xs) >>= (\xs' -> return (step xs' ps)))

down lim primes fs = step fs primes where
    step xs (p:ps) = fromMaybe (step xs ps) (try' lim p xs)

--foldem :: Factors -> Int -> Factors
--foldem xs n = if next /= xs && good next then foldem next n else xs
--    where next = try n xs

remove :: Factors -> Factors -> Factors
remove xs ys = filter ((/= 0) . snd) $ M.toList $ foldl step (M.fromList xs) ys where
    step m (b, e) = M.insertWith (+) b (- e) m

add :: Factors -> Int -> Factors
add xs f = M.toList $ M.insertWith (+) f 1 (M.fromList xs)

foo :: Int -> Factors -> (Factors, Int, Int, Bool)
foo lim xs = (xs, house xs, presents xs, good lim xs)

try :: Int -> Factors -> Factors
try p xs = next where
    next = if not (null ds) then add (remove xs (head ds)) p else xs
    ds = dropWhile ((<= p) . house) $ divisors xs

try' lim p xs = if good lim next then (Just next) else Nothing where
    next = try p xs

xdivisors :: Factors -> [Int]
xdivisors = (1 :) . foldl step [] . map powers where
    step a b = sort (nub (a ++ b ++ ((*) <$> a <*> b)))
    powers (n, c) = [ n ^ i | i <- [1 .. c] ]

factors :: Int -> M.Map Int Int
factors n = snd (foldl step (n, M.empty) (takeWhile (<= n) allPrimes)) where
    step (x, fs) p = if x `mod` p == 0 then step ((x `div` p), (M.insertWith (+) p 1 fs)) p else (x, fs)

(.>) a n = a >>= try' n

--ok :: Int -> Factors -> Bool
--ok lim xs = foo lim xs where
--    foo [] _ = False
--    foo (x:xs) n = n >= lim || foo xs (n + x)


main :: IO ()
main = do
  n <- options "Foo" (argInt  "min" "Minimum number of presents")
  print $ house (search n)
