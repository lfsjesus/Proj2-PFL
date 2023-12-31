module Lexer where

import Types
import Data.Char (isDigit, isAlpha, isSpace)

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