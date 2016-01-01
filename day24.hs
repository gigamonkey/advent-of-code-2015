import Control.Applicative

puzzle = [1, 2, 3, 7, 11, 13, 17, 19, 23, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113]

shortestWays 0 _ _ _  = [[]]
shortestWays _ _ _ [] = []
shortestWays n c u (x:xs) | n < x = []
                          | c > u = []
                          | otherwise = with <|> filter ((<= minLength) . length) without
                          where
                            with     = [ x : rest | rest <- shortestWays (n - x) (c + 1) newUpper xs ]
                            without  = shortestWays n c u xs
                            newUpper = if null without then u else length (head without)
                            minLength = if null with then newUpper else length (head with)

solve p n = minimum $ map product $ shortestWays each 0 upper p where
    each = sum p `div` n
    upper = length p

main = do
  print $ solve puzzle 3
  print $ solve puzzle 4
