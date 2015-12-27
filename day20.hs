import Data.List (find)

puzzle = 29000000

zeros = repeat 0

elf n = cycle $ n * 10 : take (n - 1) zeros

combine (x:xs) ys = x : zipWith (+) xs ys

presents = foldr combine zeros [ elf n | n <- [1 .. ] ]

main = print $ find ((>= puzzle) . snd) (zip [1..] presents)
