{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.List
import qualified Control.Foldl as F
import Data.Text (unpack)

replacements = map (head . match p) `fmap` fold (input "puzzles/day19.replacements") F.list where
    p = (,) <$> some letter <* " => " <*> some letter

start = (unpack . head) `fmap` fold (input "puzzles/day19.start") F.list

allReplacements rs s = nub [ replace f t a b | (f, t) <- rs, (a, b) <- splits, isPrefixOf f b ] where
    replace f t a b = a ++ t ++ drop (length f) b
    splits = zip (inits s) (tails s)

main = do
  rs <- replacements
  s <- start
  print $ length $ allReplacements rs s
