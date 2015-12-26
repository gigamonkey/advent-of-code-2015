{-# LANGUAGE OverloadedStrings #-}

import Turtle
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
  n <- "Sue " *> decimal <* ": "
  cs <- choice (map labeled labels) `sepBy` ", "
  return (AuntSue n (M.fromList cs))

labeled label = do { l <- text label; ": "; n <- decimal; return (l, n) }

ok aunt = all matchingCharacteristic matching where
    matchingCharacteristic (l, f) = maybe True f (M.lookup l (characteristics aunt))

main = puzzle >>= print . number . head . filter ok . map (head . match aunt)
