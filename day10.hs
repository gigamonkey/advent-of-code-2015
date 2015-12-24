seed = "3113322113"

-- 0th element is the original seed. Nth element is the result of N iterations.
lookandsay s = s : lookandsay (look 1 (head s) (tail s)) where
    look n c []     = say n c
    look n c (x:xs) = if x == c then look (n + 1) c xs else say n c ++ look 1 x xs
    say n c         = show n ++ [c]

main = print $ (length (lookandsay seed !! 40), length (lookandsay seed !! 50))
