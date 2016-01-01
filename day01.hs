puzzle = readFile "puzzles/day01.puzzle"

solve n [] = n
solve n ('(' : xs) = solve (n + 1) xs
solve n (')' : xs) = solve (n - 1) xs

solve' (-1) i _ = i
solve' n i ('(' : xs) = solve' (n + 1) (i + 1) xs
solve' n i (')' : xs) = solve' (n - 1) (i + 1) xs

main = do
  p <- puzzle
  print $ solve 0 p
  print $ solve' 0 0 p
