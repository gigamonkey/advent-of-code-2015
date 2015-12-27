{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
puzzle = B.readFile "d12.json"

puzzleJson = liftM (fromJust . decode) puzzle

total (Object o) = if hasRed o then 0 else sum $ HM.map total o
total (Array  a) = sum $ V.map total a 
total (Number n) = n
total _ = 0

hasRed = elem "red" . HM.elems

main = puzzleJson >>= print . total
