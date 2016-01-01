{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

puzzle = B.readFile "puzzles/day12.puzzle"

puzzleJson = liftM (fromJust . decode) puzzle

total (Object o) = if hasRed o then 0 else sum $ M.map total o
total (Array  a) = sum $ V.map total a
total (Number n) = n
total _ = 0

hasRed = elem "red" . M.elems

main = puzzleJson >>= print . total
