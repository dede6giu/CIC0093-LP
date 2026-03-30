main = do
    print "Testando 'uniaoRepetida'"
    print ( teste [1,2,3] [4,5,6,7] [1,2,3,4,5,6,7] )
    print ( teste [1,2,2] [1,3,4] [1,2,2,1,3,4] )
    print ( teste ['a','e'] ['i','a'] ['a','e','i','a'] )
    print ( teste [True] [False,True,False] [True,False,True,False] )

-- Resolução do exercício
uniaoRepetida x y = x ++ y


-- Funções de teste
testeCaso x y esp = uniaoRepetida x y == esp

teste x y esp | testeCaso x y esp = "PASSOU"
              | otherwise         = "FALHA: esp " ++ show esp ++ " obtido " ++ show (uniaoRepetida x y)