main = do
    print "Testando 'notaParaMencao'"
    print ( teste 10   "SS" )
    print ( teste 9.5  "SS" )
    print ( teste 9    "SS" )
    print ( teste 7.5  "MS" )
    print ( teste 7    "MS" )
    print ( teste 6    "MM" )
    print ( teste 5    "MM" )
    print ( teste 4.88 "MI" )
    print ( teste 3    "MI" )
    print ( teste 2.3  "II" )
    print ( teste 0.1  "II" )
    print ( teste 0    "SR" )

-- Resolução do exercício
notaParaMencao :: Float -> String
notaParaMencao x | 10 >= x && x >= 9 = "SS"
                 | 9 > x && x >= 7   = "MS"
                 | 7 > x && x >= 5   = "MM"
                 | 5 > x && x >= 3   = "MI"
                 | 3 > x && x > 0    = "II"
                 | x == 0            = "SR"
                 | otherwise         = "Nota inválida"


-- Funções de teste
testeCaso x esp = notaParaMencao x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (notaParaMencao x)