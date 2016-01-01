import Control.Monad

puzzle = liftM lines (readFile "puzzles/day02.puzzle")

dims :: String -> (Int, Int, Int)
dims d = (read l, read w, read h) where
    (l, rest)  = span (/= 'x') d
    (w, rest') = span (/= 'x') (tail rest)
    h          = tail rest'

sides (l, w, h) = [ (l, w), (w, h), (h, l) ]

area (a, b) = a * b

perimeter (a, b) = 2 * (a + b)

paper ds = (2 * sum ss) + minimum ss where
    ss = map area (sides ds)

ribbon ds = minimum ps + volume ds where
    ps = map perimeter (sides ds)

totalPaper = sum . map (paper . dims)

totalRibbon = sum . map (ribbon . dims)

volume (l, w, h) = l * w * h

main = do
  p <- puzzle
  print $ totalPaper p
  print $ totalRibbon p
