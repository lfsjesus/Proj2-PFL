import Data.List (sortBy)
-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Define the variable types of Stack
data StackDataType = IntValue Integer | TT | FF deriving (Show, Eq)


-- Define the types Stack and State
type Stack = [StackDataType]
type State = [(String, StackDataType)]

createEmptyStack :: Stack
createEmptyStack = []

convertFromStackStr :: StackDataType -> String
convertFromStackStr (IntValue x) = show x
convertFromStackStr TT = "True"
convertFromStackStr FF = "False"

stack2Str :: Stack -> String   --[1,2,3,4] -> "4,3,2,1"
stack2Str [] = ""
stack2Str stack = init . foldl (\acc elem ->  convertFromStackStr elem ++ "," ++ acc) "" $ stack

createEmptyState :: State
createEmptyState = []


-- Helper function to compare pairs by their first element (variable name)
compareFst :: Ord a => (a, b) -> (a, b) -> Ordering
compareFst (a1, _) (a2, _) = compare a1 a2

-- Function to convert a State to a String using foldr
state2Str :: State -> String
state2Str [] = ""  -- Handle the empty case explicitly to avoid tail call on empty string
state2Str state = 
  tail . foldr (\(key, value) acc -> "," ++ key ++ "=" ++ convertFromStackStr value ++ acc) "" $ 
  sortBy compareFst state

test1 = stack2Str [IntValue 1, IntValue 2, TT]
test2 = state2Str [("x", IntValue 1), ("z", IntValue 2), ("y", TT)]

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state) 
run ((Push elem):code, stack, state) = run (code, pushElem elem stack, state)
run ((Add):code, stack, state) = run (code, add stack, state)
run ((Mult):code, stack, state) = run (code, mult stack, state)
run ((Sub):code, stack, state) = run (code, sub stack, state)
run ((Tru):code, stack, state) = run (code, true stack, state)
run ((Fals):code, stack, state) = run (code, false stack, state)

pushElem :: Integer -> Stack -> Stack
pushElem elem stack = (IntValue elem):stack

add :: Stack -> Stack -- verify if the stack has at least 2 Integers at the top
add ((IntValue elem1):(IntValue elem2):stack) = (IntValue (elem1 + elem2)):stack
add _ = error "Run-time error"

mult :: Stack -> Stack -- verify if the stack has at least 2 Integers at the top
mult ((IntValue elem1):(IntValue elem2):stack) = (IntValue (elem1 * elem2)):stack
mult _ = error "Run-time error"

sub :: Stack -> Stack -- verify if the stack has at least 2 Integers at the top
sub ((IntValue elem1):(IntValue elem2):stack) = (IntValue (elem1 - elem2)):stack
sub _ = error "Run-time error"

true :: Stack -> Stack
true stack = TT:stack

false :: Stack -> Stack
false stack = FF:stack






-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
--testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
  --where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")