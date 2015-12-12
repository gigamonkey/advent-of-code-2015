import Data.List (group)
import System.IO

puzzle = openFile "day5.puzzle" ReadMode >>= hGetContents >>= return . lines

(&&&) f g x = f x && g x

pairs [] = []
pairs (x:[]) = []
pairs (x:y:xs) = [x,y] : pairs (y:xs)

threeVowels       = (> 2) . length . filter (`elem` "aeiou")
atLeastOneDoubled = any ((> 1) . length) . group
noNaughty         = not . any (`elem` ["ab", "cd", "pq", "xy"]) . pairs

nice = threeVowels &&& atLeastOneDoubled &&& noNaughty

-- part 2

doubled [] = False
doubled (x:[]) = False
doubled (x:xs) = (x `elem` tail xs) || (doubled xs)

doubledPair :: Eq a => [a] -> Bool
doubledPair = doubled . pairs

separated (x:[]) = False
separated (x:y:[]) = False
separated (x:y:xs) = x == head xs || separated (y:xs)

nice' = doubledPair &&& separated

main = puzzle >>= print . length . (filter nice')
