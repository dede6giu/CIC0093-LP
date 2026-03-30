main = do
    print "Testando 'funct'"
    print ( teste 1 1 0 )

-- Resolução do exercício
funct x y = 0


-- Funções de teste
testeCaso x y esp = funct x y == esp

teste x y esp | testeCaso x y esp = "PASSOU"
              | otherwise         = "FALHA: esp " ++ show esp ++ " obtido " ++ show (funct x y)