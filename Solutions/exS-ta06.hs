main = do
    print "Testando 'mapTree'"
    print ( teste 
                (*2)
                ( Node 1 ( Node 2 NilT NilT ) ( Node (-1) NilT ( Node 0 NilT NilT ) ) )
                ( Node 2 ( Node 4 NilT NilT ) ( Node (-2) NilT ( Node 0 NilT NilT ) ) ) )
    print ( teste 
                (++ "a")
                ( Node "a"  ( Node "b"  NilT NilT ) ( Node "c"  NilT ( Node "d"  NilT NilT ) ) )
                ( Node "aa" ( Node "ba" NilT NilT ) ( Node "ca" NilT ( Node "da" NilT NilT ) ) ) )
    print ( teste (+4) NilT NilT )

-- Resolução do exercício
data Tree t = NilT
            | Node t (Tree t) (Tree t)
            deriving (Eq, Ord, Show)

mapTree _ NilT = NilT
mapTree f (Node v t1 t2) = Node (f v) (mapTree f t1) (mapTree f t2)

-- Funções de teste
testeCaso x y esp = mapTree x y == esp

teste x y esp | testeCaso x y esp = "PASSOU"
              | otherwise         = "FALHA: esp " ++ show esp ++ " obtido " ++ show (mapTree x y)
