{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Vector as V
import qualified Control.Foldl as F
import Control.Monad

data CPU = CPU { registerA :: Int, registerB :: Int, pc :: Int } deriving (Show)

reboot = CPU 0 0 0

-- microcode
setA fn cpu            = cpu { registerA = fn $ registerA cpu }
setB fn cpu            = cpu { registerB = fn $ registerB cpu }
jump offset cpu        = cpu { pc = pc cpu + offset }
condJump fn offset cpu = jump (if fn cpu then offset else 1) cpu

-- op combinators
regOp label fn = liftM (\r -> r fn . jump 1) $ label *> " " *> choice [ra, rb] where
    ra = "a" *> return setA
    rb = "b" *> return setB

jmpOp label fn = liftM fn $ label *> " " *> signed decimal

jmpOpR label fn = op <$> (label *> " " *> reg <* ", ") <*> offset where
    op r   = condJump (fn . r)
    reg    = choice [ra, rb]
    offset = signed decimal
    ra     = "a" *> return registerA
    rb     = "b" *> return registerB

-- ops
ops = [ regOp "hlf" (`div` 2)
      , regOp "tpl" (* 3)
      , regOp "inc" (+ 1)
      , jmpOp "jmp" jump
      , jmpOpR "jie" even
      , jmpOpR "jio" (== 1)
      ]

compile xs = V.fromList $ map (head . match (choice ops)) xs

run cpu prog = cpu : (if halted then [] else run nextCPU prog) where
    halted  = pc cpu < 0 || pc cpu >= V.length prog
    nextCPU = op cpu
    op      = prog V.! pc cpu

puzzle = fold (input "puzzles/day23.puzzle") F.list

main = do
  p <- puzzle
  (print . registerB . last . run reboot . compile) p
  (print . registerB . last . run (CPU 1 0 0) . compile) p
