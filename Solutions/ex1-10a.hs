main = do
    print "Testando 'sumTree'"
    print ( teste ( Node 1 ( Node 2 NilT NilT ) ( Node (-1) NilT ( Node 0 NilT NilT ) ) ) 2 )
    print ( teste ( Node 2 ( Node 8 (Node (-3) NilT NilT) NilT ) ( Node 14 NilT NilT) ) 21 )
    print ( teste NilT 0 )

-- Resolução do exercício
data Tree t = NilT
            | Node t (Tree t) (Tree t)
            deriving (Eq, Ord, Show)

sumTree = foldTree (+) 0 

foldTree _ s NilT = s
foldTree f s (Node v t1 t2) = f (f v (foldTree f s t1)) (foldTree f s t2)

-- Funções de teste
testeCaso x esp = sumTree x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (sumTree x)