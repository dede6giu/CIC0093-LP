main = do
    print "Testando 'ordDec'"
    print ( teste [1,2,3,4,5] [5,4,3,2,1] )
    print ( teste ["lorem", "ipsum", "dolor", "et"] ["lorem", "ipsum", "et", "dolor"] )
    print ( teste [11] [11] )
    print ( teste [11,11,11,11,11] [11] )
    print ( teste ["aax", "aav", "abb"] ["abb", "aax", "aav"] )
    print ( teste [41,48,31,62,92,43,17,2] [92,62,48,43,41,31,17,2] )

-- Resolução do exercício
ordDec x = ordDecrescente (limpaDuplicado x)

ordDecrescente [] = []
ordDecrescente (x:xs) | maiorElem x xs = x : ordDecrescente xs
                      | otherwise      = ordDecrescente (xs ++ [x])

maiorElem _ [] = True
maiorElem x (y:ys) | x >= y    = maiorElem x ys
                   | otherwise = False 

limpaDuplicado [] = []
limpaDuplicado (x:xs) | itemAemB x xs = limpaDuplicado xs
                      | otherwise     = x : limpaDuplicado xs

itemAemB _ [] = False
itemAemB a (b:bs) | a == b    = True
                  | otherwise = itemAemB a bs


-- Funções de teste
testeCaso x esp = ordDec x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (ordDec x)