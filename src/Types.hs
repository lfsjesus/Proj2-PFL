module Types where

--PART 1--

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackDataType = IntValue Integer | BoolValue Bool deriving (Show, Eq)

type Stack = [StackDataType]
type State = [(String, StackDataType)]

-- This function creates an empty stack.
createEmptyStack :: Stack
createEmptyStack = []

-- This function creates an empty state.
createEmptyState :: State
createEmptyState = []

--PART 2--

data Aexp = Num Integer | Var String| AddAexp Aexp Aexp | SubAexp Aexp Aexp | MultAexp Aexp Aexp deriving (Show, Eq)

data Bexp = BoolBexp Bool | NegBexp Bexp | EquNumBexp Bexp Bexp | EquBoolBexp Bexp Bexp | LeNumBexp Bexp Bexp | AndBexp Bexp Bexp | AexpBexp Aexp deriving (Show,Eq) 

data Stm = AssignStm String Aexp | IfStm Bexp [Stm] [Stm] | WhileStm Bexp [Stm] deriving Show 

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