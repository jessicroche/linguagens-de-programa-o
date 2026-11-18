module TypeChecker where 

import Lexer

inferType :: Expr -> Either String Ty 
inferType (Num _) = Right TInt 
inferType (Boolean _) = Right TBool 
inferType (Sum e1 e2) = do 
    t1 <- inferType e1 
    t2 <- inferType e2 
    if t1 == TInt && t2 == TInt 
      then Right TInt 
      else Left "Erro: Soma exige dois inteiros"
-- Fazer para Mult
inferType (Equal e1 e2) = do 
    t1 <- inferType e1
    t2 <- inferType e2 
    if t1 == TInt && t2 == TInt 
      then Right TBool 
      else Left "Erro: Comparação exige dois inteiros"
inferType (If e1 e2 e3) = do 
    tCond <- inferType e1 
    tThen <- inferType e2 
    tElse <- inferType e3 
    if tCond /= TBool 
      then Left "Erro: a condição do 'if' deve ser Booleana"
      else if tThen /= tElse 
             then Left "Erro: Ramos do 'if' devem ter o mesmo tipo"
             else Right tThen 

typecheck :: Expr -> Expr 
typecheck e = case inferType e of 
                Right _  -> e 
                Left err -> error ("Type error: " ++ err)
