module Assembler where

import Types

-- This function executes the given Code until completion, returning the final Code, Stack, and State after the instructions have been executed
run :: (Code, Stack, State) -> (Code, Stack, State) 
run ([], stack, state) = ([], stack, state) 
run ((Push elem):code, stack, state) = run (code, pushElem elem stack, state)
run ((Add):code, stack, state) = run (code, add stack, state)
run ((Mult):code, stack, state) = run (code, mult stack, state)
run ((Sub):code, stack, state) = run (code, sub stack, state)
run ((Tru):code, stack, state) = run (code, true stack, state)
run ((Fals):code, stack, state) = run (code, false stack, state)
run ((Equ):code, stack, state) = run (code, equ stack, state)
run ((Le):code, stack, state) = run (code, le stack, state)
run ((And):code, stack, state) = run (code, stackAnd stack, state)
run ((Neg):code, stack, state) = run (code, neg stack, state)
run ((Fetch var):code, stack, state) = run (code, (fetchElem var stack state), state)
run ((Store var):code, stack, state) = run (code, tail stack, storeElem var stack state)
run ((Noop):code, stack, state) = run (code, stack, state) -- Don't need function for this
run ((Branch code1 code2):code, stack, state) = run (branch code1 code2 stack ++ code, tail stack, state)
run ((Loop code1 code2):code, stack, state) = run (loop code1 code2 ++ code, stack, state)

-- This function pushes an Integer element onto the stack.
pushElem :: Integer -> Stack -> Stack
pushElem elem stack = (IntValue elem):stack

-- This function adds the top two IntValues on the stack, replacing them with their sum.
add :: Stack -> Stack 
add ((IntValue elem1):(IntValue elem2):stack) = (IntValue (elem1 + elem2)):stack
add _ = error "Run-time error"

-- This function multiplies the top two IntValues on the stack, replacing them with their product.
mult :: Stack -> Stack 
mult ((IntValue elem1):(IntValue elem2):stack) = (IntValue (elem1 * elem2)):stack
mult _ = error "Run-time error"

-- This function subtracts the top two IntValues on the stack, replacing them with their difference. Especially, the second topmost element is subtracted from the topmost element.
sub :: Stack -> Stack 
sub ((IntValue elem1):(IntValue elem2):stack) = (IntValue (elem1 - elem2)):stack
sub _ = error "Run-time error"

-- This function pushes a BoolValue onto the stack with the value True.
true :: Stack -> Stack
true stack = (BoolValue True):stack

-- This function pushes a BoolValue onto the stack with the value False.
false :: Stack -> Stack
false stack = (BoolValue False):stack

-- This function compares the top two elements on the stack for equality and pushes onto the stack the boolean result.
equ :: Stack -> Stack
equ ((IntValue elem1):(IntValue elem2):stack)
  | elem1 == elem2 = (BoolValue True):stack
  | otherwise = (BoolValue False):stack
equ ((BoolValue elem1):(BoolValue elem2):stack)
  | elem1 == elem2 = (BoolValue True):stack
  | otherwise = (BoolValue False):stack
equ _ = error "Run-time error" 


-- This function compares if the second top IntValue is less than or equal to the top IntValue on the stack and pushes the boolean result.
le :: Stack -> Stack
le ((IntValue elem1):(IntValue elem2):stack)
  | elem1 <= elem2 = (BoolValue True):stack
  | otherwise = (BoolValue False):stack
le _ = error "Run-time error"

-- This function performs a logical AND on the top two BoolValues on the stack, replacing them with the result.
stackAnd :: Stack -> Stack
stackAnd ((BoolValue elem1):(BoolValue elem2):stack)
  | elem1 && elem2 = (BoolValue True):stack
  | otherwise = (BoolValue False):stack
stackAnd _ = error "Run-time error"

-- This function negates the top BoolValue on the stack, replacing it with the result.
neg :: Stack -> Stack
neg ((BoolValue elem):stack)
  | elem = (BoolValue False):stack
  | otherwise = (BoolValue True):stack
neg _ = error "Run-time error"

-- This function chooses between two branches of code based on the top BoolValue of the stack.
branch :: Code -> Code -> Stack -> Code
branch code1 code2 ((BoolValue elem):stack)
  | elem = code1
  | otherwise = code2
branch _ _ _ = error "Run-time error"

-- This function implements a while loop, executing the second code block while the first code block evaluates to True.
loop :: Code -> Code -> Code 
loop code1 code2 = code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]]

-- This function fetches the value of a variable from the state and pushes it onto the stack.
fetchElem :: String -> Stack -> State -> Stack
fetchElem varName stack state = case lookup varName state of
  Just (IntValue elem) -> pushElem elem stack
  Just (BoolValue elem) -> case elem of
    True -> true stack
    False -> false stack
  Nothing -> error "Run-time error" -- 

-- This function stores the topmost element of the stack into the state with the given variable name.
storeElem :: String -> Stack -> State -> State
storeElem varName ((IntValue elem):stack) state = (varName, (IntValue elem)) : filter ((/= varName) . fst) state
storeElem varName ((BoolValue elem):stack) state = (varName, (BoolValue elem)) : filter ((/= varName) . fst) state
storeElem _ [] state = error "Run-time error"
