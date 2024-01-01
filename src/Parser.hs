module Parser where

import Types
import Lexer

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


parseAexpOrBexpOrParen :: [Token] -> Maybe (Bexp, [Token])
parseAexpOrBexpOrParen tokens =
  case parseBexpOrParen tokens of
    Just (expr, rest) -> Just (expr, rest)
    Nothing -> case parseOperationsOrParen tokens of
      Just (expr, rest) -> Just (AexpBexp expr, rest)
      Nothing -> Nothing

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
parseSingleStm :: [Token] -> Maybe ([Stm], [Token])
parseSingleStm (VarToken varName : AssignToken : rest)
  = case parseOperationsOrParen rest of
      Just (expr, (SemicolonToken : rest2)) -> 
        Just ([AssignStm varName expr], rest2)
      _ -> Nothing

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
    -- Case when 'then' block is explicitly started with an open parenthesis '('.
    Just (expr, ThenToken : LeftParToken : rest2) ->
      case parseStatement rest2 of
        Just (stmts1, RightParToken : ElseToken : LeftParToken : rest3) ->
          case parseStatement rest3 of
            Just (stmts2, RightParToken : SemicolonToken: rest4) ->
              case parseStatement rest4 of -- Additional statements following the 'if-then-else'.
                Just (otherStmts, finalRestTokens) ->
                    Just (IfStm expr stmts1 stmts2 : otherStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        Just (stmts1, RightParToken : ElseToken : rest3) ->
          case parseSingleStm rest3 of
            Just (stmts2, rest4) ->
              case parseStatement rest4 of
                Just (otherStmts, finalRestTokens) ->
                    Just (IfStm expr stmts1 stmts2 : otherStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing

    -- Case when 'then' block is not explicitly started with an open parenthesis.
    Just (expr, ThenToken : rest2) ->
      -- Parse the 'then' block as a single statement.
      case parseSingleStm rest2 of
        Just (stmts1, ElseToken : LeftParToken : rest3) ->
          case parseStatement rest3 of
            Just (stmts2, RightParToken: SemicolonToken : rest4) -> -- Parsed else between parentheses, followed by semicolon.
              case parseStatement rest4 of -- Additional statements following the 'if-then-else'.
                Just (otherStmts, finalRestTokens) ->
                    Just (IfStm expr stmts1 stmts2 : otherStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        -- Case when 'else' follows directly after a single 'then' statement.
        Just (stmts1, ElseToken : rest3) ->
          case parseSingleStm rest3 of
            Just (stmts2, rest4) ->
              case parseStatement rest4 of -- Additional statements following the 'if-then-else'.
                Just (otherStmts, finalRestTokens) ->
                    Just (IfStm expr stmts1 stmts2 : otherStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    _ -> Nothing

---------------------------- WHILE Stm ----------------------------------
parseStatement (WhileToken : rest) =
  case parseBoolOperations rest of
    Just (expr, DoToken : LeftParToken : rest2) -> -- Case when 'do' block is explicitly enclosed in parentheses.
      case parseStatement rest2 of
        Just (stmts, RightParToken : SemicolonToken : rest3) ->
          case parseStatement rest3 of -- Additional statements following the 'while-do'.
            Just (otherStmts, finalRestTokens) ->
                Just (WhileStm expr stmts : otherStmts, finalRestTokens)
            Nothing -> Nothing
        Nothing -> Nothing

    -- Case when 'do' block is not explicitly started with an open parenthesis.
    Just (expr, DoToken : rest2) ->
      case parseSingleStm rest2 of
        Just (stmts, rest3) ->
          case parseStatement rest3 of -- Additional statements following the 'while-do'.
            Just (otherStmts, finalRestTokens) ->
                Just (WhileStm expr stmts : otherStmts, finalRestTokens)
            Nothing -> Nothing
        Nothing -> Nothing
    _ -> Nothing

parseStatement tokens = Just ([], tokens) 

buildData :: [Token] -> Program
buildData tokens = 
  case parseStatement tokens of
    Just (stms, []) -> stms
    _ -> error "Error parsing program"

parse :: String -> Program
parse = buildData . myLexer
