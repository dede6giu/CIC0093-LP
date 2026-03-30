main = do
    print "Testando 'estaDec'"
    print ( teste [5,4,3,2,1] True )
    print ( teste [11,7,5,3,2] True )
    print ( teste ["lorem", "ipsum", "et", "dolor"] True )
    print ( teste [1,2,3,4,5] False )
    print ( teste [92,76,58,43,22,9,90] False )
    print ( teste [92,43,56,34,28,18,0] False )
    print ( teste ["vamos", "descobrir", "a", "correção"] False)

-- Resolução do exercício
estaDec [] = True
estaDec [a] = True
estaDec (a:b:s) | a >= b    = estaDec (b:s)
                | otherwise = False


-- Funções de teste
testeCaso x esp = estaDec x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (estaDec x)