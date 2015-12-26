{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Text (unpack)
import Data.Tuple (swap)
import Turtle
import qualified Control.Foldl as F

replacements = map (head . match p) `fmap` fold (input "puzzles/day19.replacements") F.list where
    p = (,) <$> some letter <* " => " <*> some letter

start = (unpack . head) `fmap` fold (input "puzzles/day19.start") F.list

allReplacements rs s = nub [ replace f t a b | (f, t) <- rs, (a, b) <- splits, isPrefixOf f b ] where
    replace f t a b = a ++ t ++ drop (length f) b
    splits = zip (inits s) (tails s)

generate rs (steps, molecule) = nextstep ++ (concatMap (generate rs) nextstep) where
    nextstep = nub [ (steps + 1, next) | next <- allReplacements rs molecule ]

search molecule = Data.List.find ((== molecule) . snd)

flipped = map swap

main = do
  rs <- replacements
  s <- start
  print $ search "e" $ generate (flipped rs) (0, s)
