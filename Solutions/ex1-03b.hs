main = do
    print "Testando 'itrscLista'"
    print ( teste [0,1,2,3,4,5] [0,2,4] [0,2,4] )
    print ( teste [1,3,5] [0,1,2,3,4,5] [1,3,5] )
    print ( teste [32,47,93,29] [17,29,43,47] [47,29] )
    print ( teste ["abra", "abb", "baba"] ["baba", "kadabra", "baa", "abb"] ["abb", "baba"] )
    print ( teste [(2,1),(0,0),(-1,4)] [(2,0),(8,2),(-1,4)] [(-1,4)] )
    print ( teste ["Lorem", "Ipsum"] ["Rato", "Roeu"] [] )

-- Resolução do exercício
itrscLista [] y = []
itrscLista (x:xs) y | itemAemB x y = x : itrscLista xs y
                    | otherwise    = itrscLista xs y

itemAemB _ [] = False
itemAemB a (b:bs) | a == b    = True
                  | otherwise = itemAemB a bs

-- Funções de teste
testeCaso x y esp = itrscLista x y == esp

teste x y esp | testeCaso x y esp = "PASSOU"
              | otherwise         = "FALHA: esp " ++ show esp ++ " obtido " ++ show (itrscLista x y)