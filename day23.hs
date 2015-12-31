{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Vector as V
import qualified Control.Foldl as F

data CPU = CPU { registerA :: Int, registerB :: Int, pc :: Int } deriving (Show)

type Op = CPU -> CPU

reboot = CPU 0 0 0

-- microcode
setA fn cpu            = cpu { registerA = fn $ registerA cpu }
setB fn cpu            = cpu { registerB = fn $ registerB cpu }
jump offset cpu        = cpu { pc = pc cpu + offset }
condJump fn offset cpu = jump (if fn cpu then offset else 1) cpu

-- op combinators
regOp label fn = label *> " " *> choice [ra, rb] >>= return . (\r -> (r fn . jump 1)) where
    ra = "a" *> return setA
    rb = "b" *> return setB

jmpOp label fn = label *> " " *> signed decimal >>= return . fn

jmpOpR label fn = op <$> (label *> " " *> reg <* ", ") <*> offset where
    op r o = condJump (fn . r) o
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
    halted = (pc cpu) < 0 || (pc cpu) >= V.length prog
    nextCPU = op cpu
    op      = prog V.! (pc cpu)

puzzle = fold (input "puzzles/day23.puzzle") F.list

main = puzzle >>= print . registerB . last . run reboot . compile
