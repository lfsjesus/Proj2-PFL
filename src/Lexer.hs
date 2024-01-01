module Lexer where

import Types
import Data.Char (isDigit, isAlpha, isSpace)


-- This function tokenizes an input string into a sequence of Tokens recognized by the language.
-- It processes the input character by character, identifying numbers, keywords, variable names, and symbols.
-- The lexer handles different token types through specialized helper functions like numLexer, stringLexer, and symbolLexer.
myLexer :: String -> [Token]
myLexer [] = []
myLexer (char:rest)
  | isDigit char = numLexer (char:rest)
  | isAlpha char = stringLexer (char:rest)
  | isSpace char = myLexer rest
  | otherwise = symbolLexer (char:rest)

-- This function converts sequences of digits into NumToken. Using the span function, it identifies the longest sequence of digits and converts it into a NumToken.
numLexer :: String -> [Token]
numLexer chars = NumToken (read num) : myLexer rest
  where (num, rest) = span isDigit chars

-- This function identifies language keywords (like 'if', 'then', 'else', etc.) and variable names, converting them into respective Tokens.
-- Just like numLexer, it uses the span function to identify the longest sequence of characters that form a keyword or variable name.
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

-- This function handles individual symbols and symbol sequences, converting them into Tokens like AssignToken, AddToken, etc.
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