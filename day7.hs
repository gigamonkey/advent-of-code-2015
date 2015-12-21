{-# LANGUAGE OverloadedStrings #-}

import System.IO.Unsafe
import Control.Monad
import Data.Bits
import Data.Maybe
import System.IO
import Turtle
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Control.Foldl as L

data Atom = Number Int | Variable Text deriving (Show)

data Expr = Value Atom
          | Not Atom
          | And Atom Atom
          | Or Atom Atom
          | Rshift Atom Int
          | Lshift Atom Int
            deriving (Show)

puzzle = liftM (map T.pack . lines) (openFile "puzzles/day7.puzzle" ReadMode >>= hGetContents)

identifier = plus letter

arrow = spaces *> "->" *> spaces

variable = liftM Variable identifier

number = liftM Number decimal

atom = variable <|> number

value = liftM Value atom

notOp = liftM Not ("NOT" *> spaces *> atom)

booleanOp = do
  x <- atom
  spaces
  op <- ("AND" *> pure And) <|> ("OR" *> pure Or)
  spaces
  y <- atom
  return (op x y)

shiftOp = do
  var <- atom
  spaces
  op <- ("RSHIFT" *> pure Rshift) <|> ("LSHIFT" *> pure Lshift)
  spaces
  amt <- decimal
  return (op var amt)

line = do
  lhs <- notOp <|> booleanOp <|> shiftOp <|> value
  arrow
  var <- identifier
  return (lhs, var)

parseLine = head . match line

compile = foldl step M.empty . map parseLine where
    step m (lhs, var) = M.insert var lhs m

eval :: M.Map Text Expr -> M.Map Text Int -> Expr -> (Int, M.Map Text Int)
eval defs computed (Not a) = (complement a', computed') where
    (a', computed') = evalAtom defs computed a

eval defs computed (And a b) = (a' .&. b', withB) where
    (a', withA) = evalAtom defs computed a
    (b', withB) = evalAtom defs withA b

eval defs computed (Or a b) = (a' .|. b', withB) where
    (a', withA) = evalAtom defs computed a
    (b', withB) = evalAtom defs withA b

eval defs computed (Rshift a i) = (shift a' (- i), withA) where
    (a', withA) = evalAtom defs computed a

eval defs computed (Lshift a i) = (shift a' i, withA) where
    (a', withA) = evalAtom defs computed a

eval defs computed (Value a) = evalAtom defs computed a

evalAtom :: M.Map Text Expr -> M.Map Text Int -> Atom -> (Int, M.Map Text Int)
evalAtom defs computed (Variable v) = fromMaybe (r, M.insert v r computed') maybeComputed where
    maybeComputed = fmap (\i -> (i, computed)) (M.lookup v computed)
    (r, computed') = eval defs computed $ fromJust $ M.lookup v defs

evalAtom _ m (Number i) = (i, m)

part1 = run "a" . compile

part2 = run "a" . rewire . compile where
    rewire m = M.insert "b" (Value (Number (run "a" m))) m

run v m =  fst $ evalAtom m M.empty (Variable v)

main = do
  p <- puzzle
  print $ part1 p
  print $ part2 p
