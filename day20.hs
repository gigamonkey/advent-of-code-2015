import Data.List
import Data.Numbers.Primes

puzzle = 29000000

divisors = (1 :) . map product . nub . tail . subsequences . primeFactors

presents = (* 10) . sum . divisors

presents' = (* 11) . sum . lazyElves where
    lazyElves p = filter ((>= p) . (* 50)) $ divisors p

main = do
  print $ find ((>= puzzle) . presents) [1 ..]
  print $ find ((>= puzzle) . presents') [1 ..]
