{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.Maybe (fromMaybe)
import Data.List (nub, permutations, minimumBy, maximumBy)
import Data.Ord (comparing)
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M

matching = [
 ("children", (== 3)),
 ("cats", (> 7)),
 ("samoyeds", (== 2)),
 ("pomeranians", (< 3)),
 ("akitas", (== 0)),
 ("vizslas", (== 0)),
 ("goldfish", (< 5)),
 ("trees", (> 3)),
 ("cars", (== 2)),
 ("perfumes", (== 1))]

data AuntSue = AuntSue { number :: Int, characteristics :: M.Map Text Int } deriving (Show)

puzzle = fold (input "puzzles/day16.puzzle") F.list

labels = [ "children", "cats", "samoyeds", "pomeranians", "akitas", "vizslas", "goldfish", "trees", "cars", "perfumes"]

aunt = do
  number <- "Sue " *> decimal <* ": "
  characteristics <- choice (map labeled labels) `sepBy` ", "
  return (AuntSue number (M.fromList characteristics))

labeled label = do { l <- text label; ": "; n <- decimal; return (l, n) }

ok (AuntSue n c) = all matchingCharacteristic matching where
    matchingCharacteristic (l, f) = fromMaybe True (M.lookup l c >>= Just . f)

main = puzzle >>= print . number . head . (filter ok) . map (head . match aunt)
