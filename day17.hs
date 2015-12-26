{-# LANGUAGE OverloadedStrings #-}

eggnog = 150

sizes = [50, 44, 11, 49, 42, 46, 18, 32, 26, 40, 21, 7, 18, 43, 10, 47, 36, 24, 22, 40]

ways 0 _      = [[]]
ways _ []     = []
ways n (x:xs) = [ x : tails | tails <- ways (n - x) xs ] ++ ways n xs

main = print $ length $ filter ((== fewest) . length) allways where
    allways = ways eggnog sizes
    fewest = minimum $ map length allways
