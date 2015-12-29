import Data.List
import Data.Numbers.Primes

presents = sum . map product . nub . tail . subsequences . primeFactors

main = print $ find ((>= 2900000) . presents) [1 ..]
