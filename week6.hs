--RECURSIVE DATA TYPES:

data Exp = Val Int | Add Exp Exp | Sub Exp Exp | Mul Exp Exp | Div Exp Exp 
    deriving Eq

instance Show Exp where 
    show :: Exp -> String
    show (Val n) = show n
    show (Add expl expr) = "(" ++ show expl ++ " + " ++ show expr ++ ")"
    show (Sub expl expr) = "(" ++ show expl ++ " - " ++ show expr ++ ")"
    show (Mul expl expr) = "(" ++ show expl ++ " * " ++ show expr ++ ")"
    show (Div expl expr) = "(" ++ show expl ++ " / " ++ show expr ++ ")"

eval :: Exp -> Int 
eval (Val n)        = n 
eval (Add e1 e2)    = eval e1 + eval e2
eval (Sub e1 e2)    = eval e1 - eval e2
eval (Mul e1 e2)    = eval e1 * eval e2
eval (Div e1 e2)    = eval e1 `div` eval e2
