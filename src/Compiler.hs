module Compiler where

import Types

-- This function compiles an arithmetic expression (Aexp) into a sequence of instructions (Code).
compA (Num elem) = [Push elem]
compA (Var varName) = [Fetch varName]
compA (AddAexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Add]
compA (SubAexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Sub]
compA (MultAexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Mult]

-- This function compiles a boolean expression (Bexp) into a sequence of instructions (Code).
compB :: Bexp -> Code
compB (BoolBexp elem)
  | elem = [Tru]
  | otherwise = [Fals]
compB (AexpBexp elem) = compA elem
compB (NegBexp elem) = compB elem ++ [Neg]
compB (EquNumBexp elem1 elem2) = compB elem2 ++ compB elem1 ++ [Equ]
compB (EquBoolBexp elem1 elem2) = compB elem2 ++ compB elem1 ++ [Equ]
compB (LeNumBexp elem1 elem2) = compB elem2 ++ compB elem1 ++ [Le]
compB (AndBexp elem1 elem2) = compB elem2 ++ compB elem1 ++ [And]

-- This is the main compile function, which compiles a program (list of statements) into a sequence of instructions (Code),
-- which will then be executed by the run function in Assembler.hs.
-- It calls compA and compB to compile the arithmetic and boolean expressions, respectively.
compile :: Program -> Code
compile [] = []
compile ((AssignStm varName elem):rest) = compA elem ++ [Store varName] ++ compile rest
compile ((IfStm bool stm1 stm2):rest) = compB bool ++ [Branch (compile stm1) (compile stm2)] ++ compile rest
compile ((WhileStm bool stm):rest) = [Loop (compB bool) (compile stm)] ++ compile rest