import Data.List (sortBy)
import Data.Char 
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

data Bexp = BoolBexp Bool | NegBexp Bexp | EquNumBexp Aexp Aexp | EquBoolBexp Bexp Bexp | LeNumBexp Aexp Aexp | AndBexp Bexp Bexp | NumBexp Integer | VarBexp String deriving Show 

data Stm = AssignStm String Aexp | IfStm Bexp [Stm] [Stm] | WhileStm Bexp [Stm] | NoopStm | Seq [Stm] deriving Show 

type Program = [Stm]

data Token
  = NumToken Integer
  | VarToken String
  | AddToken
  | SubToken
  | MultToken
  | TrueToken
  | FalseToken
  | AndToken
  | NegToken
  | WhileToken
  | DoToken
  | IfToken
  | ThenToken
  | ElseToken
  | AssignToken
  | EquNumToken
  | EquBoolToken
  | LeEquToken
  | SemicolonToken
  | LeftParToken
  | RightParToken
  deriving (Show)


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
compB (NumBexp elem) = [Push elem]
compB (VarBexp varName) = [Fetch varName]
compB (NegBexp elem) = compB elem ++ [Neg]
compB (EquNumBexp elem1 elem2) = compA elem1 ++ compA elem2 ++ [Equ]
compB (EquBoolBexp elem1 elem2) = compB elem1 ++ compB elem2 ++ [Equ]
compB (LeNumBexp elem1 elem2) = compA elem1 ++ compA elem2 ++ [Le]
compB (AndBexp elem1 elem2) = compB elem1 ++ compB elem2 ++ [And]

-- compile
compile :: Program -> Code
compile [] = []
compile ((AssignStm varName elem):rest) = compA elem ++ [Store varName] ++ compile rest
compile ((IfStm bool stm1 stm2):rest) = compB bool ++ [Branch (compile stm1) (compile stm2)] ++ compile rest
compile ((WhileStm bool stm):rest) = [Loop (compB bool) (compile stm)] ++ compile rest
compile ((NoopStm):rest) = [Noop] ++ compile rest
compile ((Seq stm):rest) = compile stm ++ compile rest
-- lexer


myLexer :: String -> [Token]
myLexer [] = []
myLexer (char:rest)
  | isDigit char = numLexer (char:rest)
  | isAlpha char = stringLexer (char:rest)
  | isSpace char = myLexer rest
  | otherwise = symbolLexer (char:rest)

numLexer :: String -> [Token]
numLexer chars = NumToken (read num) : myLexer rest
  where (num, rest) = span isDigit chars

stringLexer :: String -> [Token]
stringLexer chars = 
  let (word, rest) = span isAlpha chars
  in case word of 
    "True" -> TrueToken : myLexer rest
    "False" -> FalseToken : myLexer rest
    "and" -> AndToken : myLexer rest
    "not" -> NegToken : myLexer rest
    "while" -> WhileToken : myLexer rest
    "do" -> DoToken : myLexer rest
    "if" -> IfToken : myLexer rest
    "then" -> ThenToken : myLexer rest
    "else" -> ElseToken : myLexer rest
    _ -> VarToken word : myLexer rest

symbolLexer :: String -> [Token]
symbolLexer(':':'=':rest) = AssignToken : myLexer rest
symbolLexer ('=':'=':rest) = EquNumToken : myLexer rest
symbolLexer ('<':'=':rest) = LeEquToken : myLexer rest
symbolLexer ('=':rest) = EquBoolToken : myLexer rest
symbolLexer ('+':rest) = AddToken : myLexer rest
symbolLexer ('-':rest) = SubToken : myLexer rest
symbolLexer ('*':rest) = MultToken : myLexer rest
symbolLexer ('(':rest) = LeftParToken : myLexer rest
symbolLexer (')':rest) = RightParToken : myLexer rest
symbolLexer (';':rest) = SemicolonToken : myLexer rest
symbolLexer _ = error "Lexical error"

{-
buildData :: [Token] -> Program
buildData [] = []
buildData tokens = 
  let (stm, restTokens) = parseStm tokens
  in stm : buildData restTokens
-}

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp (NumToken num : rest)
  = Just (Num num, rest)
parseAexp (VarToken varName : rest)
  = Just (Var varName, rest)
parseAexp tokens
  = Nothing

-- parentesis
parseAexpOrParen :: [Token] -> Maybe (Aexp, [Token])
parseAexpOrParen (LeftParToken : rest)
  = case parseAexpOrParen rest of
      Just (expr, (RightParToken : rest2)) -> Just (expr, rest2)
      _ -> Nothing
parseAexpOrParen tokens
  = parseAexp tokens

parseMultOrAexpOrParen :: [Token] -> Maybe (Aexp, [Token])
parseMultOrAexpOrParen tokens 
  = case parseAexpOrParen tokens of
      Just (expr1, (MultToken : rest)) -> 
        case parseMultOrAexpOrParen rest of
        Just (expr2, rest2) -> 
            Just (MultAexp expr1 expr2, rest2)
        Nothing -> Nothing
      result -> result

parseOperationsOrParen :: [Token] -> Maybe (Aexp, [Token])
parseOperationsOrParen tokens 
  = case parseMultOrAexpOrParen tokens of
      Just (expr1, (AddToken : rest)) -> 
        case parseOperationsOrParen rest of
        Just (expr2, rest2) -> 
            Just (AddAexp expr1 expr2, rest2)
        Nothing -> Nothing
      Just (expr1, (SubToken : rest)) -> 
        case parseOperationsOrParen rest of
        Just (expr2, rest2) -> 
            Just (SubAexp expr1 expr2, rest2)
        Nothing -> Nothing
      result -> result


parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp (LeftParToken : rest)
  = case parseBoolOperations rest of
      Just (expr, (RightParToken : rest2)) -> 
        Just (expr, rest2)
      _ -> Nothing
      Nothing -> Nothing
parseBexp (TrueToken : rest)
  = Just (BoolBexp True, rest)
parseBexp (FalseToken : rest)
  = Just (BoolBexp False, rest)
parseBexp (NumToken num : rest)
  = Just (NumBexp num, rest)
parseBexp (VarToken varName : rest)
  = Just (VarBexp varName, rest)
parseBexp _ = Nothing

parseLeOrBexp :: [Token] -> Maybe (Bexp, [Token])
parseLeOrBexp tokens 
  = case parseBexp tokens of
      Just (expr1, (LeEquToken : rest)) -> 
        case parseBexp rest of
        Just (expr2, rest2) -> 
            Just (LeNumBexp expr1 expr2, rest2)
        Nothing -> Nothing
      result -> result

parseLeOrEquOrBexp :: [Token] -> Maybe (Bexp, [Token])
parseLeOrEquOrBexp tokens 
  = case parseLeOrBexp tokens of
      Just (expr1, (EquNumToken : rest)) -> 
        case parseLeOrEquOrBexp rest of
        Just (expr2, rest2) -> 
            Just (EquNumBexp expr1 expr2, rest2)
        Nothing -> Nothing
      result -> result

parseLeOrEquOrNegOrBexp :: [Token] -> Maybe (Bexp, [Token])
parseLeOrEquOrNegOrBexp tokens 
  = case tokens of
      (NegToken : rest) -> 
        case parseLeOrEquOrNegOrBexp rest of
        Just (expr, rest2) -> 
            Just (NegBexp expr, rest2)
        Nothing -> Nothing
      _ -> parseLeOrEquOrBexp tokens

parseLeOrEquOrNegOrBoolEquOrBexp :: [Token] -> Maybe (Bexp, [Token])
parseLeOrEquOrNegOrBoolEquOrBexp tokens 
  = case parseLeOrEquOrNegOrBexp tokens of
      Just (expr1, (EquBoolToken : rest)) -> 
        case parseLeOrEquOrNegOrBoolEquOrBexp rest of
        Just (expr2, rest2) -> 
            Just (EquBoolBexp expr1 expr2, rest2)
        Nothing -> Nothing
      result -> result

parseBoolOperations :: [Token] -> Maybe (Bexp, [Token])
parseBoolOperations tokens 
  = case parseLeOrEquOrNegOrBoolEquOrBexp tokens of
      Just (expr1, (AndToken : rest)) -> 
        case parseBoolOperations rest of
        Just (expr2, rest2) -> 
            Just (AndBexp expr1 expr2, rest2)
        Nothing -> Nothing
      result -> result

parseSequence :: [Token] -> Maybe ([Stm], [Token])


parseStmSeq :: [Token] -> Maybe ([Stm], [Token])
parseStmSeq tokens = loopAux tokens []

-- Auxiliary loop function defined outside parseStmSeq
loopAux :: [Token] -> [Stm] -> Maybe ([Stm], [Token])
loopAux [] acc = Just (reverse acc, [])
loopAux (ElseTok : rest) acc = Just (reverse acc, ElseTok : rest)
loopAux tokens acc =
  case parseStm tokens of
    Just (stm, restTokens) ->
      case restTokens of
        (SemicolonTok : restTokens') -> loopAux restTokens' (stm : acc)
        _ -> Just (reverse (stm : acc), restTokens)
    Nothing -> Nothing



-- parse :: String -> Program
-- parse = buildData . myLexer


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
--testParser programCode = (stack2Str stack, state2Str state)
--  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")