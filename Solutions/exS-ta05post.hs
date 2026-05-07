main = do
    print "Testando 'collapse'"
    print ( teste ( Node 1 ( Node 2 NilT NilT ) ( Node 4 NilT ( Node 3 NilT NilT ) ) ) 
                [2,3,4,1] )
    print ( teste ( Node 1 ( Node 2 NilT NilT ) ( Node 4 (Node 5 NilT NilT) ( Node 3 NilT NilT ) ) ) 
                [2,5,3,4,1] )
    print ( teste ( Node 1 ( Node 2 ( Node 3 ( Node 4 NilT NilT ) NilT ) NilT ) NilT ) 
                [4,3,2,1] )
    print ( teste ( Node "t" ( Node "l" NilT NilT ) (Node "r" NilT NilT ) ) 
                ["l","r","t"] )

-- Resolução do exercício
data Tree t = NilT
            | Node t (Tree t) (Tree t)
            deriving (Eq, Ord, Show)

collapse NilT = []
collapse (Node v t1 t2) = collapse t1 ++ collapse t2 ++ [v]

-- Funções de teste
testeCaso x esp = collapse x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (collapse x)