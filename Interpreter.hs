import Lexer
import Parser

eval :: Expr -> Int 
eval (Num n) = n 
eval (Sum e1 e2) = 
    let v1 = eval e1 
        v2 = eval e2 
      in v1 + v2 
eval (Mult e1 e2) = 
    let v1 = eval e1 
        v2 = eval e2 
      in v1 * v2 

eval (Sub e1 e2) = 
    let v1 = eval e1 
        v2 = eval e2 
      in v1 - v2 

step :: Expr -> Expr
--SOMA
step(Sum(Num n1) (Num n2)) = Num (n1+n2)
step(Sum (Num n1) e2) = 
    let e2' = step e2
        in Sum(Num n1) e2'
step(Sum e1 e2) = 
    let e1' = step e1
        in Sum e1' e2
--MULT
step(Mult(Num n1) (Num n2)) = Num (n1*n2)
step(Mult (Num n1) e2) = 
    let e2' = step e2
        in Mult(Num n1) e2'
step(Mult e1 e2) = 
    let e1' = step e1
        in Mult e1' e2

-- SUB
step(Sub(Num n1) (Num n2)) = Num (n1-n2)
step(Sub (Num n1) e2) = 
    let e2' = step e2
        in Sub(Num n1) e2'
step(Sub e1 e2) = 
    let e1' = step e1
        in Sub e1' e2

-- DIV
step(Div(Num n1) (Num n2)) = Num (n1 `div` n2)
step(Div (Num n1) e2) = 
    let e2' = step e2
        in Div(Num n1) e2'
step(Div e1 e2) = 
    let e1' = step e1
        in Div e1' e2


step e = e

execute :: Expr -> [Expr]
execute e | step e == e = [e]
          | otherwise = e : execute (step e)