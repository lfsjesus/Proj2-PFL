module AuxFunctions where

import Types
import Data.List (sortBy)

-- This function converts a StackDataType (IntValue or BoolValue) to a String representation.
convertFromStackStr :: StackDataType -> String
convertFromStackStr (IntValue elem) = show elem
convertFromStackStr (BoolValue elem) = show elem

-- This function converts a Stack to a String representation. It iterates over the stack and calls convertFromStackStr on each element.
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str stack = init . foldr (\elem acc ->  convertFromStackStr elem ++ "," ++ acc) "" $ stack

-- This is an auxiliary function which compares two tuples by their first element. We use it to sort the state by the variable name.
compareFst :: Ord a => (a, b) -> (a, b) -> Ordering
compareFst (a1, _) (a2, _) = compare a1 a2

-- This function converts a State to a sorted String representation, iterating over the state and converting each element to a String.
state2Str :: State -> String
state2Str [] = ""
state2Str state = 
  tail . foldr (\(key, value) acc -> "," ++ key ++ "=" ++ convertFromStackStr value ++ acc) "" $ 
  sortBy compareFst state