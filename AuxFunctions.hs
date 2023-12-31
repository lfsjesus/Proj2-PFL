module AuxFunctions where

import Types
import Data.List (sortBy)

convertFromStackStr :: StackDataType -> String
convertFromStackStr (IntValue elem) = show elem
convertFromStackStr (BoolValue elem) = show elem

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str stack = init . foldr (\elem acc ->  convertFromStackStr elem ++ "," ++ acc) "" $ stack

compareFst :: Ord a => (a, b) -> (a, b) -> Ordering
compareFst (a1, _) (a2, _) = compare a1 a2

state2Str :: State -> String
state2Str [] = ""
state2Str state = 
  tail . foldr (\(key, value) acc -> "," ++ key ++ "=" ++ convertFromStackStr value ++ acc) "" $ 
  sortBy compareFst state