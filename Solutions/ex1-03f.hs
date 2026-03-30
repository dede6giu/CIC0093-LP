main = do
    print "Testando 'nItem'"
    print ( teste 1 [3,4,8,5,9] 4 )
    print ( teste 4 [3,4,8,5,9] 9 )
    print ( teste 0 [0,1,2] 0 )
    print ( teste 1 [0,1,2] 1 )
    print ( teste 2 [0,1,2] 2 )
    print ( teste 1 ["abra","kadabra"] "kadabra")

-- Resolução do exercício
nItem 0 (x:xs) = x
nItem n (x:xs) = nItem (n-1) xs


-- Funções de teste
testeCaso n x esp = nItem n x == esp

teste n x esp | testeCaso n x esp = "PASSOU"
              | otherwise         = "FALHA: esp " ++ show esp ++ " obtido " ++ show (nItem n x)