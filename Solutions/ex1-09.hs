main = do
    print "Testando 'foldTree'"
    print ( teste (+) 0 
                ( Node 1 ( Node 2 NilT NilT ) ( Node (-1) NilT ( Node 0 NilT NilT ) ) ) 
                2 )
    print ( teste (++) "empty" NilT "empty" )
    print ( teste (++) "" 
                ( Node "I" ( Node "Love" (Node "Programming" NilT NilT) NilT ) ( Node "Code" NilT NilT) )
                "ILoveProgrammingCode" )

-- Resolução do exercício
data Tree t = NilT
            | Node t (Tree t) (Tree t)
            deriving (Eq, Ord, Show)

foldTree _ s NilT = s
foldTree f s (Node v t1 t2) = f (f v (foldTree f s t1)) (foldTree f s t2)

-- Funções de teste
testeCaso x y z esp = foldTree x y z == esp

teste x y z esp | testeCaso x y z esp = "PASSOU"
                | otherwise           = "FALHA: esp " ++ show esp ++ " obtido " ++ show (foldTree x y z)