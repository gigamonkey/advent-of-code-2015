import System.IO

puzzle = openFile "puzzles/day1.puzzle" ReadMode >>= hGetContents

solve n [] = n
solve n ('(' : xs) = solve (n + 1) xs
solve n (')' : xs) = solve (n - 1) xs

solve' (-1) i _ = i
solve' n i ('(' : xs) = solve' (n + 1) (i + 1) xs
solve' n i (')' : xs) = solve' (n - 1) (i + 1) xs

main = puzzle >>= print . solve' 0 0
