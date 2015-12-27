import Control.Applicative
import Data.List

puzzle = 29000000

zeros = repeat 0

elf n = cycle $ n : take (n - 1) zeros

combine (x:xs) ys = x : zipWith (+) xs ys

presents = foldr combine zeros [ elf n | n <- [1 .. ] ]

lim = puzzle `div` 10

main = print $ find ((>= lim) . snd) (zip [1..] presents)
