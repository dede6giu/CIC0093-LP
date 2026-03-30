main = do
    print "Testando 'inverteLista'"
    print ( teste [1,2,3,4] [4,3,2,1] )
    print ( teste ['a','e','i','o','u'] ['u','o','i','e','a'] )
    print ( teste [42] [42] )
    print ( teste [True, True, False] [False, True, True] )
    print ( teste [0,0,0,0,1,0,0,0,0] [0,0,0,0,1,0,0,0,0] )
    print ( teste [0,0,1,0,0,1,0,0,0] [0,0,0,1,0,0,1,0,0] )

-- Resolução do exercício
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]


-- Funções de teste
testeCaso x esp = inverteLista x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (inverteLista x)