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

parseStatement (IfToken : restTokens1) =
  -- Parse the condition of the 'if' statement.
  case parseBoolOperations restTokens1 of
    -- Case when 'then' block is explicitly started with an open parenthesis '('.
    Just (expr, ThenToken : LeftParToken : restTokens2) ->
      case parseStatement restTokens2 of
        -- Case when both 'then' and 'else' blocks are explicitly enclosed in parentheses.
        Just (stmts1, RightParToken : ElseToken : LeftParToken : restTokens3) ->
          case parseStatement restTokens3 of
            Just (stmts2, RightParToken : restTokens4) ->
              -- Parse additional statements following the 'if-then-else'.
              case parseStatement restTokens4 of
                Just (additionalStmts, finalRestTokens) ->
                    Just ([IfStm expr stmts1 stmts2] ++ additionalStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        Just (stmts1, RightParToken : ElseToken : restTokens3) ->
          case parseSingleStm restTokens3 of
            Just (stmts2, restTokens4) ->
              case parseStatement restTokens4 of
                Just (additionalStmts, finalRestTokens) ->
                    Just ([IfStm expr stmts1 stmts2] ++ additionalStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing

    -- Case when 'then' block is not explicitly started with an open parenthesis.
    Just (expr, ThenToken : restTokens2) ->
      -- Parse the 'then' block as a single statement.
      case parseSingleStm restTokens2 of
        -- Case when 'else' block starts with an open parenthesis after a single 'then' statement.
        Just (stmts1, ElseToken : LeftParToken : restTokens3) ->
          -- Parse the 'else' block.
          case parseStatement restTokens3 of
            -- Successfully parsed 'else' block, expecting closing parenthesis.
            Just (stmts2, RightParToken: SemicolonToken : restTokens4) -> 
              -- Parse additional statements following the 'if-then-else'.
              case parseStatement restTokens4 of
                -- Successfully parsed additional statements.
                Just (additionalStmts, finalRestTokens) ->
                    Just ([IfStm expr stmts1 stmts2] ++ additionalStmts, finalRestTokens)
                -- Failed to parse additional statements.
                Nothing -> Nothing
            -- Failed to parse 'else' block.
            Nothing -> Nothing
        -- Case when 'else' follows directly after a single 'then' statement.
        Just (stmts1, ElseToken : restTokens3) ->
          -- Parse the 'else' block as a single statement.
          case parseSingleStm restTokens3 of
            -- Successfully parsed single 'else' statement.
            Just (stmts2, restTokens4) ->
              -- Parse additional statements following the 'if-then-else'.
              case parseStatement restTokens4 of
                -- Successfully parsed additional statements.
                Just (additionalStmts, finalRestTokens) ->
                    Just ([IfStm expr stmts1 stmts2] ++ additionalStmts, finalRestTokens)
                -- Failed to parse additional statements.
                Nothing -> Nothing
            -- Failed to parse 'else' block.
            Nothing -> Nothing
        -- Failed to parse 'then' block as a single statement.
        Nothing -> Nothing
    -- Failed to parse the condition or didn't find the expected 'ThenTok'.
    _ -> Nothing

---------------------------- WHILE Stm ----------------------------------
parseStatement (WhileToken : restTokens1) =
  -- Parse the condition of the 'while' statement.
  case parseBoolOperations restTokens1 of
    -- Case when 'do' block is explicitly started with an open parenthesis '('.
    Just (expr, DoToken : LeftParToken : restTokens2) ->
      case parseStatement restTokens2 of
        -- Case when 'do' block is explicitly enclosed in parentheses.
        Just (stmts, RightParToken : SemicolonToken : restTokens3) ->
          -- Parse additional statements following the 'while-do'.
          case parseStatement restTokens3 of
            Just (additionalStmts, finalRestTokens) ->
                Just ([WhileStm expr stmts] ++ additionalStmts, finalRestTokens)
            Nothing -> Nothing
        Nothing -> Nothing

    -- Case when 'do' block is not explicitly started with an open parenthesis.
    Just (expr, DoToken : restTokens2) ->
      -- Parse the 'do' block as a single statement.
      case parseSingleStm restTokens2 of
        -- Successfully parsed single 'do' statement.
        Just (stmts, restTokens3) ->
          -- Parse additional statements following the 'while-do'.
          case parseStatement restTokens3 of
            -- Successfully parsed additional statements.
            Just (additionalStmts, finalRestTokens) ->
                Just ([WhileStm expr stmts] ++ additionalStmts, finalRestTokens)
            -- Failed to parse additional statements.
            Nothing -> Nothing
        -- Failed to parse 'do' block.
        Nothing -> Nothing
    -- Failed to parse the condition or didn't find the expected 'DoTok'.
    _ -> Nothing

parseStatement tokens = Just ([], tokens) 

buildData :: [Token] -> Program
buildData tokens = 
  case parseStatement tokens of
    Just (stms, []) -> stms
    _ -> error "Error parsing program"

parse :: String -> Program
parse = buildData . myLexer
