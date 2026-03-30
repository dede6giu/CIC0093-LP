main = do
    print "Testando 'difLista'"
    print ( teste [1,1,1] [1,1,1] [] )
    print ( teste [3,2,1] [1,1,1] [3,2] )
    print ( teste [1,1,1] [3,2,1] [] )
    print ( teste [32,32,32,32,32] [32,64,0,2,8] [] )
    print ( teste [56,92,70,41] [45,15,4,27] [56,92,70,41] )
    print ( teste [0] [-1] [0] )
    print ( teste [True, False] [False] [True] )
    print ( teste ['a','a','b','c','d'] ['d','x','c'] ['a','a','b'] )
    print ( teste ["dif", "entr", "conj"] ["mas", "nem", "sao", "conj"] ["dif", "entr"] )

-- Resolução do exercício
difLista [] _ = []
difLista (x:xs) y | itemAemB x y = difLista xs y
                  | otherwise    = x : difLista xs y

itemAemB _ [] = False
itemAemB a (b:bs) | a == b    = True
                  | otherwise = itemAemB a bs

-- Funções de teste
testeCaso x y esp = difLista x y == esp

teste x y esp | testeCaso x y esp = "PASSOU"
              | otherwise         = "FALHA: esp " ++ show esp ++ " obtido " ++ show (difLista x y)