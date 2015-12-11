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

main = puzzle >>= print . length . (filter nice)
