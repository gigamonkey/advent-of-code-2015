-- Tweet-length version of solution to part 1 of day 20.
import Data.List
import Data.Numbers.Primes
main = print $ find ((>=2900000).sum.(1:).map product.nub.tail.subsequences.primeFactors) [1..]
