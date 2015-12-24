import Control.Applicative
import Data.List
import Data.Char

start = "cqjxjnds"

(&&&) a b = (&&) <$> a <*> b

increment = reverse . rincrement . reverse

rincrement (x:xs) = if x == 'z' then 'a' : rincrement xs else (chr $ ord x + 1) : xs

straight []       = []
straight [x]      = [x]
straight (x:y:xs) = if ord x + 1 == ord y then x : straight (y:xs) else [x]

threeStraight [] = False
threeStraight xs = length (straight xs) >= 3 || threeStraight (tail xs)

nonumlike = null . intersect "iol"

pairs :: Ord a => Int -> [a] -> Int
pairs n [] = n
pairs n [x] = n
pairs n (x:y:xs) = if x == y then pairs (n + 1) xs else pairs n (y:xs)

twoPairs :: String -> Bool
twoPairs = (>= 2) . pairs 0

ok = threeStraight &&& nonumlike &&& twoPairs

next s = head $ filter ok (tail (iterate increment s))

main = putStrLn $ next $ next start
