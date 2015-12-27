import Control.Applicative
import Data.List
import qualified Data.Map.Strict as M
import qualified Control.Foldl as F

puzzle = 29000000

-- primes = fold (input "primes.txt") F.list

start :: [(Int, Int)]
start = [(2,21)]

primes :: [Int]
primes = 2 : filter isPrime [3 .. ] where
    isPrime n = not (any ((== 0) . (n `mod`)) $ takeWhile ((<= n) . (^ 2)) primes)

a :: [(Int, Int)] -> Int
a = sum . divisors

divisors :: [(Int, Int)] -> [Int]
divisors = (1 :) . foldl step [] . map powers where
    step a b = nub (a ++ b ++ ((*) <$> a <*> b))
    powers (n, c) = [ n ^ i | i <- [1 .. c] ]

expand :: [(Int, Int)] -> Int
expand = product . map (\(b, e) -> b ^ e)

factors :: Int -> [(Int, Int)]
factors n = sort $ M.toList $ snd (foldl step (n, M.empty) (takeWhile (<= n) primes)) where
    step (x, fs) p = if x `mod` p == 0 then step ((x `div` p), (M.insertWith (+) p 1 fs)) p else (x, fs)

foo :: [(Int, Int)] -> ([(Int, Int)], Int, Int, Bool)
foo xs = (xs, expand xs, a xs, good xs)

-- Given a prime, find the smallest divisor greater than p. Divide xs by that divisor and then

try p xs = add (remove xs (factors $ head ds)) p where
    ds = dropWhile (<= p) $ sort $ divisors xs

good = (>= (puzzle `div` 10)) . a

--quux xs = if ok (try xs 2) then try xs 2
--    bar ys n = try ys n
--    if n == puzzle then xs else (if a (try xs n) >= puzzle then quux (try xs n) n else quux xs (n + 1))

iter xs n = tail $ takeWhile good (iterate (try n) xs)

foldem xs n = if good next then foldem next n else xs where next = try n xs

quux n = foldl step start (take n primes) where
    step xs n = foldem xs n




smallest :: [(Int, Int)] -> Int -> [(Int, Int)]
smallest spec p = error "nyi"

remove :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
remove xs ys = filter ((/= 0) . snd) $ M.toList $ foldl step (M.fromList xs) ys where
    step m (b, e) = M.insertWith (+) b (- e) m

add :: [(Int, Int)] -> Int -> [(Int, Int)]
add xs f = M.toList $ M.insertWith (+) f 1 (M.fromList xs)


log2 n = shrink n 0 where
    shrink 1 i = i
    shrink n i = shrink (n `div` 2) (i + 1)

zeros = repeat 0

elf n = cycle $ n * 10 : take (n - 1) zeros

combine (x:xs) ys = x : zipWith (+) xs ys

presents = foldr combine zeros [ elf n | n <- [1 .. ] ]

limit = puzzle `div` 10

--quux n xs = bar n (n - 1) 1 xs where
--    bar n m c (x:xs) | m > 0 = x : bar n (m - 1) c xs
--                     | otherwise = ((n,c) : x) : bar n (n - 1) (c + 1) xs

--houses :: [[(Int, Int)]]
--houses = foldr quux (repeat [(1,1)]) (takeWhile (< 1000) primes)


main = print $ find ((>= puzzle) . snd) (zip [1..] presents)
