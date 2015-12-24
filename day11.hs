import Control.Applicative
import Data.List
import Data.Char

start = "cqjxjnds"

(&&&) a b = (&&) <$> a <*> b

increment = reverse . rincrement . reverse where
    rincrement (x:xs) = if x == 'z' then 'a' : rincrement xs else chr (ord x + 1) : xs

threeStraight = (>= 3) . straight where
    straight []       = 0
    straight [x]      = 1
    straight (x:y:xs) = if ord x + 1 == ord y then max (1 + straight (y:xs)) (straight xs) else straight (y:xs)

noNumLike = null . intersect "iol"

twoPairs = (>= 2) . pairs 0 where
    pairs n [] = n
    pairs n [x] = n
    pairs n (x:y:xs) = if x == y then pairs (n + 1) xs else pairs n (y:xs)

ok = threeStraight &&& noNumLike &&& twoPairs

next s = head $ filter ok $ tail $ iterate increment s

main = putStrLn $ next $ next start
