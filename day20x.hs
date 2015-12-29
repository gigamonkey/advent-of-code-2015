{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Numbers.Primes
import Data.Ord
import Turtle
import Turtle (options, argInt)
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M

type Factors = [(Int, Int)]

puzzle :: Int
puzzle = 29000000

--lim = puzzle `div` 10

bestish, better, evenbetter, best :: Factors
bestish    = [(2,5),(3,1),(5,1),(7,1),(241,1)]
better     = [(2,3),(3,3),(5,1),(7,1),(107,1)]
evenbetter = [(2,2),(3,5),(5,1),(7,1),(23,1)]
best       = [(2,6),(3,3),(5,1),(7,1),(11,1)]

start :: Int -> Factors
start lim = [(2,floor $ logBase 2 (fromIntegral lim))]

factors :: Int -> Factors
factors = map (\x -> (head x, length x)) . group . primeFactors

number :: Factors -> Int
number = product . map (\(b, e) -> b ^ e)

divide :: Factors -> Factors -> Factors
divide xs ys = filter ((/= 0) . snd) $ M.toList $ foldl step (M.fromList xs) ys where
    step m (b, e) = M.insertWith (+) b (- e) m

mult :: Factors -> Factors -> Factors
mult xs ys = M.toList $ foldl step (M.fromList xs) ys where
    step m (f, n) = M.insertWith (+) f n m

divisors :: Factors -> [Factors]
divisors = foldl step [] . map powers where
    step a b = sortBy (comparing number) $ nub (a ++ b ++ ((++) <$> a <*> b))
    powers (n, c) = [ [(n, i)] | i <- [1 .. c] ]

presents :: Factors -> Int
presents = (+ 1) . sum . map number . divisors

good :: Int -> Factors -> Bool
good lim = (>= lim) . presents


up lim n = Data.List.find (good lim) nexts where
    p = fst $ last n
    nexts = [ (n `divide` [(p, 1)]) `mult` (factors x) | x <- [1 .. p] ]

fixpoint f x = if f x == x then x else fixpoint f (f x)

search lim = fixpoint step (start lim) where
    ps = tail (takeWhile (<= lim) primes)
    step xs = fixpoint (down lim ps) (fixpoint (across lim ps)  xs)

across lim ps fs = step fs ps where
    step xs (p:ps) = fromMaybe xs ((try lim p xs) >>= (\xs' -> return (step xs' ps)))

down lim ps fs = step fs ps where
    step xs (p:ps) = fromMaybe (step xs ps) (try lim p xs)

shrink' lim p n = Data.List.find (good lim) nexts where
    bigger = filter ((> number p) . number) $ divisors n
    nexts = [ (n `divide` d) `mult` p | d <- bigger ]

try lim p n = if good lim next then (Just next) else Nothing where
    next = if not (null d) then (n `divide` (head d)) `mult` [(p, 1)] else n
    d = dropWhile ((<= p) . number) $ divisors n

main :: IO ()
main = do
  n <- options "Foo" (argInt  "min" "Minimum number of presents")
  print $ number (search n)
