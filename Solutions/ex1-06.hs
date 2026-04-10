main = do
    print "Testando 'aprovadosOrdemDeMedia'"
    print ( teste [("a", 5, 9), ("b", 0, 10), ("c", 3, 5), ("d", 5, 5)] 
                  [("d", 5), ("b", 5), ("a", 7)] )

-- Resolução do exercício
aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia x = [(n,a) | (n,a) <- qs (notasFinais x), notaFiltro a]

notaFiltro a = a >= 5

qs [] = []
qs ((xn,xa):xs) = (qs [(n,a) | (n,a) <- xs, a <= xa]) ++ [(xn,xa)]
                ++ qs [(n,a) | (n,a) <- xs, a >  xa]

notasFinais [] = []
notasFinais ((n,a,b):ns) = (n, mediaNotas a b) : notasFinais ns

mediaNotas a b = (a+b)/2

-- Funções de teste
testeCaso x esp = aprovadosOrdemDeMedia x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (aprovadosOrdemDeMedia x)