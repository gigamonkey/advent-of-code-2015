{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Aeson
import Data.HashMap.Strict
import Data.Maybe
import qualified Data.ByteString.Lazy as B

puzzle = B.readFile "puzzles/day12.puzzle"

puzzleJson = liftM (fromJust . decode) puzzle

total (Object o) = if hasRed o then 0 else foldl' (\acc v -> acc + total v) 0 o
total (Array  a) = foldl (\acc v -> acc + total v) 0 a
total (Number n) = n
total _ = 0

hasRed = any (== "red") . elems

main = puzzleJson >>= print . total
