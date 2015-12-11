import System.IO

puzzle = openFile "day2.puzzle" ReadMode >>= hGetContents >>= return . lines

dims :: String -> (Int, Int, Int)
dims d = (read l, read w, read h) where
    (l, rest)  = span (/= 'x') d
    (w, rest') = span (/= 'x') (tail rest)
    h          = tail rest'

sides (l, w, h) = [l * w, w * h, h * l]

paper ds = (2 * (sum ss)) + minimum ss where
    ss = sides ds

total = sum . (map (paper . dims))

main = puzzle >>= print . total
