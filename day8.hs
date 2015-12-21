{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Control.Foldl as F
import qualified Data.Text as T

puzzle = input "puzzles/day8.puzzle"

codeCharacters = T.length

textCharacters = subtract 2 . stringCharacters . T.unpack

-- this assumes all the strings are properly formatted, e.g. there are
-- no stranded '\'s, etc.
stringCharacters []                  = 0
stringCharacters ('\\':'"':rest)     = 1 + stringCharacters rest
stringCharacters ('\\':'\\':rest)    = 1 + stringCharacters rest
stringCharacters ('\\':'x':_:_:rest) = 1 + stringCharacters rest
stringCharacters (x:xs)              = 1 + stringCharacters xs

main = view $ fold (fmap (\t -> codeCharacters t - textCharacters t) puzzle) F.sum
