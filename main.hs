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
data StackDataType = IntValue Integer | BoolValue Bool deriving (Show, Eq)


-- Define the types Stack and State
type Stack = [StackDataType]
type State = [(String, StackDataType)]

createEmptyStack :: Stack
createEmptyStack = []

convertFromStackStr :: StackDataType -> String
convertFromStackStr (IntValue elem) = show elem
convertFromStackStr (BoolValue elem) = show elem


stack2Str :: Stack -> String   --[1,2,3,4] -> "4,3,2,1"
stack2Str [] = ""
stack2Str stack = init . foldr (\elem acc ->  convertFromStackStr elem ++ "," ++ acc) "" $ stack

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

test1 = stack2Str [IntValue 1, IntValue 2, BoolValue True]
test2 = state2Str [("x", IntValue 1), ("z", IntValue 2), ("y", BoolValue True)]

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
run ((Branch code1 code2):code, stack, state) = run (branch code1 code2 stack, tail stack, state)
run ((Loop code1 code2):code, stack, state) = run (loop code1 code2 ++ code, stack, state)

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
true stack = (BoolValue True):stack

false :: Stack -> Stack
false stack = (BoolValue False):stack

equ :: Stack -> Stack
equ ((IntValue elem1):(IntValue elem2):stack)
  | elem1 == elem2 = (BoolValue True):stack
  | otherwise = (BoolValue False):stack
equ ((BoolValue elem1):(BoolValue elem2):stack)
  | elem1 == elem2 = (BoolValue True):stack
  | otherwise = (BoolValue False):stack
equ _ = error "Run-time error" 

le :: Stack -> Stack
le ((IntValue elem1):(IntValue elem2):stack)
  | elem1 <= elem2 = (BoolValue True):stack
  | otherwise = (BoolValue False):stack
le _ = error "Run-time error"

stackAnd :: Stack -> Stack
stackAnd ((BoolValue elem1):(BoolValue elem2):stack)
  | elem1 && elem2 = (BoolValue True):stack
  | otherwise = (BoolValue False):stack
stackAnd _ = error "Run-time error"

neg :: Stack -> Stack
neg ((BoolValue elem):stack)
  | elem = (BoolValue False):stack
  | otherwise = (BoolValue True):stack
neg _ = error "Run-time error"


branch :: Code -> Code -> Stack -> Code
branch code1 code2 ((BoolValue elem):stack)
  | elem = code1
  | otherwise = code2
branch _ _ _ = error "Run-time error"

loop :: Code -> Code -> Code 
loop code1 code2 = code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]]


-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"
-- 

fetchElem :: String -> Stack -> State -> Stack
fetchElem varName stack state = case lookup varName state of
  Just (IntValue elem) -> pushElem elem stack
  Just (BoolValue elem) -> case elem of
    True -> true stack
    False -> false stack
  Nothing -> error "Run-time error" -- 

storeElem :: String -> Stack -> State -> State
storeElem varName ((IntValue elem):stack) state = (varName, (IntValue elem)) : filter ((/= varName) . fst) state
storeElem varName ((BoolValue elem):stack) state = (varName, (BoolValue elem)) : filter ((/= varName) . fst) state
storeElem _ [] state = error "Run-time error" -- stack is empty


-- Part 2 -----------------------------------------------------------------------------------------------------------


data Aexp = Num Integer | Var String| AddAexp Aexp Aexp | SubAexp Aexp Aexp | MultAexp Aexp Aexp deriving Show

data Bexp = BoolBexp Bool | NegBexp Bexp | EquNumBexp Aexp Aexp | EquBoolBexp Bexp Bexp | LeNumBexp Aexp Aexp | AndBexp Bexp Bexp deriving Show

data Stm = AssignStm String Aexp | IfStm Bexp [Stm] [Stm] | WhileStm Bexp [Stm] | NoopStm | Aexp Aexp | Bexp Bexp deriving Show 



-- compA 
compA :: Aexp -> Code
compA (Num elem) = [Push elem]
compA (Var varName) = [Fetch varName]
compA (AddAexp elem1 elem2) = compA elem1 ++ compA elem2 ++ [Add]
compA (SubAexp elem1 elem2) = compA elem1 ++ compA elem2 ++ [Sub]
compA (MultAexp elem1 elem2) = compA elem1 ++ compA elem2 ++ [Mult]

-- compB
compB :: Bexp -> Code
compB (BoolBexp elem)
  | elem = [Tru]
  | otherwise = [Fals]
compB (NegBexp elem) = compB elem ++ [Neg]
compB (EquNumBexp elem1 elem2) = compA elem1 ++ compA elem2 ++ [Equ]
compB (EquBoolBexp elem1 elem2) = compB elem1 ++ compB elem2 ++ [Equ]
compB (LeNumBexp elem1 elem2) = compA elem1 ++ compA elem2 ++ [Le]
compB (AndBexp elem1 elem2) = compB elem1 ++ compB elem2 ++ [And]

-- compile
compile :: [Stm] -> Code
compile [] = []
compile ((AssignStm varName elem):rest) = compA elem ++ [Store varName] ++ compile rest
compile ((IfStm bool stm1 stm2):rest) = compB bool ++ [Branch (compile stm1) (compile stm2)] ++ compile rest
compile ((WhileStm bool stm):rest) = [Loop (compB bool) (compile stm)] ++ compile rest
compile ((NoopStm):rest) = [Noop] ++ compile rest
compile ((Aexp elem):rest) = compA elem ++ compile rest
compile ((Bexp elem):rest) = compB elem ++ compile rest

-- parse
parse :: String -> [Stm]
parse [] = []
parse str = parseStm (words str)

parseStm :: [String] -> [Stm]
parseStm [] = []
parseStm ("if":rest) = parseIfStm rest
parseStm ("while":rest) = parseWhileStm rest
parseStm ("noop":rest) = NoopStm : parseStm rest
parseStm (varName:"=":rest) = AssignStm varName (parseAexp rest) : parseStm rest
parseStm (varName:rest) = AssignStm varName (parseAexp rest) : parseStm rest

parseIfStm :: [String] -> [Stm]
parseIfStm [] = []
parseIfStm ("(":rest) = parseIfStm rest
parseIfStm (")":rest) = parseIfStm rest
parseIfStm ("then":rest) = parseIfStm rest
parseIfStm ("else":rest) = parseIfStm rest
parseIfStm ("endif":rest) = parseStm rest
parseIfStm ("(":rest) = parseIfStm rest
parseIfStm (")":rest) = parseIfStm rest
parseIfStm ("then":rest) = parseIfStm rest
parseIfStm ("else":rest) = parseIfStm rest
parseIfStm ("endif":rest) = parseStm rest
parseIfStm ("(":rest) = parseIfStm rest
parseIfStm (")":rest) = parseIfStm rest
parseIfStm ("then":rest) = parseIfStm rest
parseIfStm ("else":rest) = parseIfStm rest
parseIfStm ("endif":rest) = parseStm rest
parseIfStm ("(":rest) = parseIfStm rest
parseIfStm (")":rest) = parseIfStm rest
parseIfStm ("then":rest) = parseIfStm rest
parseIfStm ("else":rest) = parseIfStm rest
parseIfStm ("endif":rest) = parseStm rest
parseIfStm ("(":rest) = parseIfStm rest

parseWhileStm :: [String] -> [Stm]
parseWhileStm [] = []
parseWhileStm ("(":rest) = parseWhileStm rest
parseWhileStm (")":rest) = parseWhileStm rest
parseWhileStm ("do":rest) = parseWhileStm rest
parseWhileStm ("endwhile":rest) = parseStm rest
parseWhileStm ("(":rest) = parseWhileStm rest
parseWhileStm (")":rest) = parseWhileStm rest
parseWhileStm ("do":rest) = parseWhileStm rest
parseWhileStm ("endwhile":rest) = parseStm rest
parseWhileStm ("(":rest) = parseWhileStm rest
parseWhileStm (")":rest) = parseWhileStm rest
parseWhileStm ("do":rest) = parseWhileStm rest
parseWhileStm ("endwhile":rest) = parseStm rest
parseWhileStm ("(":rest) = parseWhileStm rest
parseWhileStm (")":rest) = parseWhileStm rest
parseWhileStm ("do":rest) = parseWhileStm rest
parseWhileStm ("endwhile":rest) = parseStm rest
parseWhileStm ("(":rest) = parseWhileStm rest

parseAexp :: [String] -> Aexp
parseAexp [] = error "Run-time error"
parseAexp (elem:rest)
  | elem == "+" = AddAexp (parseAexp rest) (parseAexp (tail rest))
  | elem == "-" = SubAexp (parseAexp rest) (parseAexp (tail rest))
  | elem == "*" = MultAexp (parseAexp rest) (parseAexp (tail rest))
  | otherwise = Num (read elem :: Integer)

parseBexp :: [String] -> Bexp
parseBexp [] = error "Run-time error"







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
-- test a branch
-- testAssembler [Fals,Branch [Push 1] [Push 2]] == ("1","")
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
-- parse :: String -> Program

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