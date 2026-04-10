main = do
    print "Testando 'quickSort'"
    print ( teste [1,2,3,4,5] [5,4,3,2,1] )
    print ( teste ["lorem", "ipsum", "dolor", "et"] ["lorem", "ipsum", "et", "dolor"] )
    print ( teste [11] [11] )
    print ( teste [11,11,11,11,11] [11] )
    print ( teste ["aax", "aav", "abb"] ["abb", "aax", "aav"] )
    print ( teste [41,48,31,62,92,43,17,2] [92,62,48,43,41,31,17,2] )

-- Resolução do exercício
quickSort x = limpaDuplicado (qsSort x)

qsSort [] = []
qsSort (x:xs) = (qsSort [a | a <- xs, a >= x]) ++ [x] ++ (qsSort [a | a <- xs, a < x])

limpaDuplicado [] = []
limpaDuplicado (x:xs) | itemAemB x xs = limpaDuplicado xs
                      | otherwise     = x : limpaDuplicado xs

itemAemB _ [] = False
itemAemB a (b:bs) | a == b    = True
                  | otherwise = itemAemB a bs

-- Funções de teste
testeCaso x esp = quickSort x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (quickSort x)