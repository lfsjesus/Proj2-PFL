module Parser where

import Types
import Lexer

-- This function parses arithmetic expressions and parentheses from a list of Tokens.
-- It handles numeric literals, variable names, and expressions within parentheses
-- by recursively calling the top-level function for parsing arithmetic expressions.
parseAexpOrParen :: [Token] -> Maybe (Aexp, [Token])
parseAexpOrParen (NumToken num : rest)
  = Just (Num num, rest)
parseAexpOrParen (VarToken varName : rest)
  = Just (Var varName, rest)
parseAexpOrParen (LeftParToken : rest)
  = case parseOperationsOrParen rest of
      Just (expr, (RightParToken : rest2)) -> 
        Just (expr, rest2)
      Just _ -> Nothing
      Nothing -> Nothing
parseAexpOrParen _ = Nothing

-- This function parses multiplication operations from a list of Tokens.
-- It handles multiplication operations and expressions within parentheses
-- by recursively calling the previous function and itself for nested expressions.
parseMultOrAexpOrParen :: [Token] -> Maybe (Aexp, [Token])
parseMultOrAexpOrParen tokens 
  = case parseAexpOrParen tokens of
      Just (expr1, (MultToken : rest)) -> 
        case parseMultOrAexpOrParen rest of
        Just (expr2, rest2) -> 
            Just (MultAexp expr1 expr2, rest2)
        Nothing -> Nothing
      Just (expr, (SemicolonToken : rest)) -> 
        Just (expr, (SemicolonToken : rest))
      result -> result  
        
-- This is the top-level function for parsing arithmetic expressions.
-- This function parses addition and subtraction operations from a list of Tokens.
-- It handles addition and subtraction operations and expressions within parentheses 
-- by recursively calling the previous function and itself for nested expressions.
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
      Just (expr, (SemicolonToken : rest)) -> 
        Just (expr, (SemicolonToken : rest))
      result -> result

----- Boolean Operations ------------------------------
  
-- This function parses boolean expressions and parentheses from a list of Tokens.
-- It handles boolean literals, and expressions within parentheses
-- by calling the top-level function for parsing boolean expressions.  
parseBexpOrParen :: [Token] -> Maybe (Bexp, [Token])
parseBexpOrParen (LeftParToken : rest)
  = case parseBoolOperations rest of
      Just (expr, (RightParToken : rest2)) -> 
        Just (expr, rest2)
      Just _ -> Nothing
      Nothing -> Nothing
parseBexpOrParen (TrueToken : rest)
  = Just (BoolBexp True, rest)
parseBexpOrParen (FalseToken : rest)
  = Just (BoolBexp False, rest)
parseBexpOrParen _ = Nothing

-- This function parses boolean and arithmetic expressions and parentheses from a list of Tokens.
-- It handles boolean and arithmetic expressions and expressions within parentheses 
-- by recursively calling the previous function for nested expressions and the top-level arithmetic function for parsing arithmetic expressions.
parseAexpOrBexpOrParen :: [Token] -> Maybe (Bexp, [Token])
parseAexpOrBexpOrParen tokens =
  case parseBexpOrParen tokens of
    Just (expr, rest) -> Just (expr, rest)
    Nothing -> case parseOperationsOrParen tokens of
      Just (expr, rest) -> Just (AexpBexp expr, rest)
      Nothing -> Nothing

-- This function parses less than or equal operations from a list of Tokens.
-- It handles less than or equal operations and expressions within parentheses 
-- by recursively calling the previous function and itself for nested expressions.
parseLeOrBexp :: [Token] -> Maybe (Bexp, [Token])
parseLeOrBexp tokens 
  = case parseAexpOrBexpOrParen tokens of
      Just (expr1, (LeEquToken : rest)) -> 
        case parseLeOrBexp rest of
        Just (expr2, rest2) -> 
            Just (LeNumBexp expr1 expr2, rest2)
        Nothing -> Nothing
      Just (expr, (SemicolonToken : rest)) -> 
        Just (expr, (SemicolonToken : rest))
      result -> result  

-- This function parses integer equal operations from a list of Tokens.
-- It handles equal operations and expressions within parentheses
-- by recursively calling the previous function and itself for nested expressions.
parseLeOrEquOrBexp :: [Token] -> Maybe (Bexp, [Token])
parseLeOrEquOrBexp tokens 
  = case parseLeOrBexp tokens of
      Just (expr1, (EquNumToken : rest)) -> 
        case parseLeOrEquOrBexp rest of
        Just (expr2, rest2) -> 
            Just (EquNumBexp expr1 expr2, rest2)
        Nothing -> Nothing
      Just (expr, (SemicolonToken : rest)) ->
        Just (expr, (SemicolonToken : rest))
      result -> result

-- This function parses negation operations from a list of Tokens.
-- It handles negation operations and expressions within parentheses
-- by recursively calling the previous function and itself for nested expressions.
parseLeOrEquOrNegOrBexp :: [Token] -> Maybe (Bexp, [Token])
parseLeOrEquOrNegOrBexp (NegToken : rest)
  = case parseLeOrEquOrBexp rest of
      Just (expr, rest2) -> 
        Just (NegBexp expr, rest2)
      result -> result
parseLeOrEquOrNegOrBexp tokens 
  = case parseLeOrEquOrBexp tokens of
      Just (expr, rest) -> Just (expr, rest)
      result -> result
         
-- This function parses boolean equal operations from a list of Tokens.
-- It handles boolean equal operations and expressions within parentheses
-- by recursively calling the previous function and itself for nested expressions.
parseLeOrEquOrNegOrBoolEquOrBexp :: [Token] -> Maybe (Bexp, [Token])
parseLeOrEquOrNegOrBoolEquOrBexp tokens 
  = case parseLeOrEquOrNegOrBexp tokens of
      Just (expr1, (EquBoolToken : rest)) -> 
        case parseLeOrEquOrNegOrBoolEquOrBexp rest of
        Just (expr2, rest2) -> 
            Just (EquBoolBexp expr1 expr2, rest2)
        Nothing -> Nothing
      Just (expr, (SemicolonToken : rest)) -> 
        Just (expr, (SemicolonToken : rest))
      result -> result

-- This is the top-level function for parsing boolean expressions.
-- This function parses and operations from a list of Tokens.
-- It handles and operations and expressions within parentheses
-- by recursively calling the previous function and itself for nested expressions.
parseBoolOperations :: [Token] -> Maybe (Bexp, [Token])
parseBoolOperations tokens 
  = case parseLeOrEquOrNegOrBoolEquOrBexp tokens of
      Just (expr1, (AndToken : rest)) -> 
        case parseBoolOperations rest of
        Just (expr2, rest2) -> 
            Just (AndBexp expr1 expr2, rest2)
        Nothing -> Nothing
      Just (expr, (SemicolonToken : rest)) ->
        Just (expr, (SemicolonToken : rest))
      result -> result


----- Statements ------------------------------


-- This function parses a single statement from a list of Tokens.
-- It handles variable names, assignment operations, and expressions within parentheses
-- by recursively calling the top-level function for parsing arithmetic expressions.
parseSingleStm :: [Token] -> Maybe ([Stm], [Token])
parseSingleStm (VarToken varName : AssignToken : rest)
  = case parseOperationsOrParen rest of
      Just (expr, (SemicolonToken : rest2)) -> 
        Just ([AssignStm varName expr], rest2)
      _ -> Nothing


-- This function parses statements from a list of Tokens.
-- It handles variable names, assignment operations, if statements, while statements and expressions within parentheses
-- by recursively calling  itself and the top-level function for parsing arithmetic expressions.
parseStatement :: [Token] -> Maybe ([Stm], [Token])
parseStatement [] = Just ([], [])
parseStatement (VarToken varName : AssignToken : rest)
  = case parseOperationsOrParen rest of
      Just (expr, (SemicolonToken : rest2)) -> 
        case parseStatement rest2 of
          Just (stms, rest3) -> 
            Just (AssignStm varName expr : stms, rest3)
          Nothing ->
            Just ([AssignStm varName expr], rest2)
      _ -> Nothing 

---------------------------- IF Stm ----------------------------------
parseStatement (IfToken : rest) =
  case parseBoolOperations rest of
    -- Case when 'then' block starts with an open parenthesis '('.
    Just (expr, ThenToken : LeftParToken : rest2) ->
      case parseStatement rest2 of
        -- Case when 'else' block starts with an open parenthesis '('.
        Just (stmts1, RightParToken : ElseToken : LeftParToken : rest3) ->
          case parseStatement rest3 of
            Just (stmts2, RightParToken : SemicolonToken: rest4) ->
              case parseStatement rest4 of -- Rest of the statements following the 'if / then / else'.
                Just (otherStmts, finalRestTokens) ->
                    Just (IfStm expr stmts1 stmts2 : otherStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        -- Case when 'else' consists of a single statement.
        Just (stmts1, RightParToken : ElseToken : rest3) ->
          -- Parse the 'else' block as a single statement.
          case parseSingleStm rest3 of
            Just (stmts2, rest4) ->
              case parseStatement rest4 of
                Just (otherStmts, finalRestTokens) ->
                    Just (IfStm expr stmts1 stmts2 : otherStmts, finalRestTokens) -- Rest of the statements following the 'if / then / else'.
                Nothing -> Nothing
            Nothing -> Nothing

    -- Case when 'then' block consists of a single statement.
    Just (expr, ThenToken : rest2) ->
      -- Parse the 'then' block as a single statement.
      case parseSingleStm rest2 of
        -- Case when 'else' block starts with an open parenthesis '('.
        Just (stmts1, ElseToken : LeftParToken : rest3) ->
          case parseStatement rest3 of
            Just (stmts2, RightParToken: SemicolonToken : rest4) -> -- Parsed else between parentheses, followed by semicolon.
              case parseStatement rest4 of -- Rest of the statements following the 'if / then / else'.
                Just (otherStmts, finalRestTokens) ->
                    Just (IfStm expr stmts1 stmts2 : otherStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        -- Case when 'else' consists of a single statement.
        Just (stmts1, ElseToken : rest3) ->
          -- Parse the 'else' block as a single statement.
          case parseSingleStm rest3 of
            Just (stmts2, rest4) ->
              case parseStatement rest4 of -- Rest of the statements following the 'if / then / else'.
                Just (otherStmts, finalRestTokens) ->
                    Just (IfStm expr stmts1 stmts2 : otherStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    _ -> Nothing

---------------------------- WHILE Stm ----------------------------------
parseStatement (WhileToken : rest) =
  case parseBoolOperations rest of
    -- Case when 'do' block is explicitly enclosed in parentheses.
    Just (expr, DoToken : LeftParToken : rest2) ->
      case parseStatement rest2 of
        Just (stmts, RightParToken : SemicolonToken : rest3) ->
          case parseStatement rest3 of -- Rest of the statements following the 'while / do'. 
            Just (otherStmts, finalRestTokens) ->
                Just (WhileStm expr stmts : otherStmts, finalRestTokens)
            Nothing -> Nothing
        Nothing -> Nothing

    -- Case when 'do' block is not explicitly started with an open parenthesis.
    Just (expr, DoToken : rest2) ->
      -- Parse the 'do' block as a single statement.
      case parseSingleStm rest2 of
        Just (stmts, rest3) ->
          case parseStatement rest3 of -- Rest of the statements following the 'while / do'.
            Just (otherStmts, finalRestTokens) ->
                Just (WhileStm expr stmts : otherStmts, finalRestTokens)
            Nothing -> Nothing
        Nothing -> Nothing
    _ -> Nothing

parseStatement tokens = Just ([], tokens) 

-- This function builds a program from a list of Tokens.
buildData :: [Token] -> Program
buildData tokens = 
  case parseStatement tokens of
    Just (stms, []) -> stms
    _ -> error "Error parsing program"

--  This function uses the lexer to convert a string into a list of Tokens and then builds a program from the list of Tokens.
parse :: String -> Program
parse = buildData . myLexer
