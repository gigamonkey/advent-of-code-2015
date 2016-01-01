import Data.List

puzzle = (2947, 3029)

codes = 20151125 : map next codes where
    next = (`mod` 33554393) . (* 252533)

coords = concat [ diagonal n | n <- [1..] ] where
    diagonal n = zip [n,n-1..1] [1..n]

main = print $ find ((== puzzle) . fst) (zip coords codes)
