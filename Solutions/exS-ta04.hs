main = do
    print "Testando 'depth'"
    print ( teste ( Node 1 ( Node 2 NilT NilT ) ( Node 2 NilT ( Node 3 NilT NilT ) ) ) 3 )
    print ( teste NilT 0 )
    print ( teste ( Node 1 ( Node 2 ( Node 3 ( Node 4 NilT NilT ) NilT ) NilT ) NilT ) 4 )

-- Resolução do exercício
data Tree t = NilT
            | Node t (Tree t) (Tree t)
            deriving (Eq, Ord, Show)

depth NilT = 0
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

-- Funções de teste
testeCaso x esp = depth x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (depth x)