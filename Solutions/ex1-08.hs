main = do
    print "Testando 'eval'"
    print ( teste (Add (Lit 1) (Lit 1)) 2 )
    print ( teste (Add (Lit 13) (Lit 17)) 30 )
    print ( teste (Sub (Lit 0) (Lit 1)) (-1) )
    print ( teste (Sub (Lit 10) (Lit 4)) 6 )
    print ( teste (Mult (Lit 1) (Lit 0)) 0 )
    print ( teste (Mult (Lit 5) (Lit 7)) 35 )
    print ( teste (Mult (Lit 4) (Add (Lit 8) (Lit (-1)))) 28 )
    print ( teste (Lit 2) 2 )

-- Resolução do exercício
data Expr = Lit Int
            | Add Expr Expr
            | Sub Expr Expr
            | Mult Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mult e1 e2) = eval e1 * eval e2

-- Funções de teste
testeCaso x esp = eval x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (eval x)