main = do
    print "Testando 'histograma'"
    print ( teste [
            "O", "Senhor", "conosco", "está", "Sim", "Ele", "está", "Ele", "está"
        ] [
            ("está",3), ("Ele",2), ("O",1), ("Senhor",1), ("Sim",1), ("conosco",1) 
        ] )

-- Resolução do exercício
histograma :: [String] -> [(String, Int)]
histograma x = ordHistDec (histConstr (ordCrescente(x)) (1))

ordHistDec [] = []
ordHistDec (x:xs) | maiorPar x xs = x : ordHistDec xs
                  | otherwise     = ordHistDec (xs ++ [x])

maiorPar _ [] = True
maiorPar (a,x) ((b,y):bs) | x >= y    = maiorPar (a,x) (bs)
                          | otherwise = False

histConstr [] _ = []
histConstr [a] n = [(a,n)]
histConstr (a:b:s) n | a == b    = histConstr (b:s) (n+1) 
                     | otherwise = [(a, n)] ++ histConstr (b:s) (1)

ordCrescente [] = []
ordCrescente (x:xs) | menorElem x xs = x : ordCrescente xs
                    | otherwise      = ordCrescente (xs ++ [x])

menorElem _ [] = True
menorElem x (y:ys) | x <= y    = menorElem x ys
                   | otherwise = False 


-- Funções de teste
testeCaso x esp = histograma x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (histograma x)