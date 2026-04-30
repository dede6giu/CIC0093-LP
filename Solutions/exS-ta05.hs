main = do
    print "Testando 'collapse'"
    print ( teste ( Node 1 ( Node 2 NilT NilT ) ( Node 2 NilT ( Node 3 NilT NilT ) ) ) 
                [1,2,2,3] )
    print ( teste ( Node 1 ( Node 2 ( Node 3 ( Node 4 NilT NilT ) NilT ) NilT ) NilT ) 
                [1,2,3,4] )
    print ( teste ( Node "t" ( Node "l" NilT NilT ) (Node "r" NilT NilT ) ) 
                ["t","l","r"] )

-- Resolução do exercício
data Tree t = NilT
            | Node t (Tree t) (Tree t)
            deriving (Eq, Ord, Show)

collapse NilT = []
collapse (Node v t1 t2) = [v] ++ collapse t1 ++ collapse t2

-- Funções de teste
testeCaso x esp = collapse x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (collapse x)