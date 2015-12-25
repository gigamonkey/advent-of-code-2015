{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Control.Foldl as F
import qualified Data.Text as T

puzzle = input "puzzles/day8.puzzle"

codeCharacters = T.length

textCharacters = subtract 2 . stringCharacters . T.unpack

textCharacters' = (+ 2) . reencodedCharacters . T.unpack

-- this assumes all the strings are properly formatted, e.g. there are
-- no stranded '\'s, etc.
stringCharacters []                = 0
stringCharacters ('\\':'"':xs)     = 1 + stringCharacters xs
stringCharacters ('\\':'\\':xs)    = 1 + stringCharacters xs
stringCharacters ('\\':'x':_:_:xs) = 1 + stringCharacters xs
stringCharacters (x:xs)            = 1 + stringCharacters xs

reencodedCharacters []        = 0
reencodedCharacters ('\\':xs) = 2 + reencodedCharacters xs
reencodedCharacters ('"':xs)  = 2 + reencodedCharacters xs
reencodedCharacters (x:xs)    = 1 + reencodedCharacters xs

sum1 t = codeCharacters t - textCharacters t
sum2 t = textCharacters' t - codeCharacters t


main = view $ fold (fmap sum2 puzzle) F.sum
