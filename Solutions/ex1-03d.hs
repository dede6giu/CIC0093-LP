main = do
    print "Testando 'uniaoLista'"
    print ( teste [1,2,3] [4,5,6,7] [1,2,3,4,5,6,7] )
    print ( teste [1,2,2] [1,3,4] [2,1,3,4] )
    print ( teste ['a','e'] ['i','a'] ['e','i','a'] )
    print ( teste [True] [False,True,False] [True,False] )
    print ( teste [1] [2] [1,2] )
    print ( teste [1] [] [1] )
    print ( teste [] [0] [0] )
    print ( teste [1,1,1,1,1,1,1,1] [1,1,1,1,1,1,1,1,1,1] [1] )
    print ( teste [2,2,2,2,2,2,2,1] [1,1,1,1,1,1,1,1,1,2] [1,2] )
    print ( teste ["kadabra", "abra", "cesame"] ["magika", "abra"] ["kadabra", "cesame", "magika", "abra"] )

-- Resolução do exercício
uniaoLista x y = limpaDuplicado (limpaDuplicado x ++ limpaDuplicado y)

limpaDuplicado [] = []
limpaDuplicado (x:xs) | itemAemB x xs = limpaDuplicado xs
                      | otherwise     = x : limpaDuplicado xs

itemAemB _ [] = False
itemAemB a (b:bs) | a == b    = True
                  | otherwise = itemAemB a bs

-- Funções de teste
testeCaso x y esp = uniaoLista x y == esp

teste x y esp | testeCaso x y esp = "PASSOU"
              | otherwise         = "FALHA: esp " ++ show esp ++ " obtido " ++ show (uniaoLista x y)