module Lexer where 

import Data.Char (isDigit, isSpace)

data Token = TokNum Int 
           | TokSum
           | TokMult
           | TokOpenPar
           | TokClosePar
           | TokSub
           | TokDiv
           deriving (Show, Eq)

data Expr = Num Int 
          | Sum Expr Expr 
          | Mult Expr Expr 
          | Sub Expr Expr
          | Div Expr Expr
          deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokSum : lexer cs 
lexer ('*':cs) = TokMult : lexer cs
lexer ('(':cs) = TokOpenPar : lexer cs
lexer (')':cs) = TokClosePar : lexer cs
lexer ('-':cs) = TokSub : lexer cs
lexer ('/':cs) = TokDiv : lexer cs
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs)
             | otherwise = error ("Erro lexico: caracter invalido '" ++ [c] ++ "'")

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
              (numStr, rest) -> TokNum (read numStr) : lexer rest 