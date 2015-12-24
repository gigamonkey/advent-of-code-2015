import Data.List (maximumBy, minimumBy, nub, permutations)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Turtle
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M

seed = "3113322113"

lookandsay s = s : lookandsay (look 0 'a' s) where
    look 0 _ (x:xs) = look 1 x xs
    look n c [] = say n c
    look n c (x:xs) = if x == c then look (n + 1) c xs else say n c ++ look 1 x xs
    say n c = show n ++ [c]

main = print $ length (lookandsay seed !! 40)
