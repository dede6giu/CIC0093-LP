main = do
    print "Testando 'difLista'"
    print ( teste [1,1,1] [1,1,1] [0,0,0] )
    print ( teste [3,2,1] [1,1,1] [2,1,0] )
    print ( teste [56,92,70,41] [45,15,4,27] [11,77,66,14] )
    print ( teste [0] [-1] [1] )
    print ( teste [1] [765] [-764] )
    print ( teste [] [] [] )
    print ( teste [32,32,32,32,32] [32,64,0,2,8] [0,-32,32,30,24] )

-- Resolução do exercício
difLista [x] [y] = [x-y]
difLista (x:xs) (y:ys) = (x-y) : difLista xs ys


-- Funções de teste
testeCaso x y esp = difLista x y == esp

teste x y esp | testeCaso x y esp = "PASSOU"
              | otherwise         = "FALHA: esp " ++ show esp ++ " obtido " ++ show (difLista x y)