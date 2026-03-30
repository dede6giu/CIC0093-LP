import Distribution.Simple.Utils (xargs)
main = do
    print "Testando 'ultimoItem'"
    print ( teste [1,2,3,4,5] 5 )
    print ( teste ['a', 'i', 'u', 'e', 'o'] 'o' )

-- Resolução do exercício
ultimoItem [x]    = x
ultimoItem (x:xs) = ultimoItem xs


-- Funções de teste
testeCaso x esp = ultimoItem x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (ultimoItem x)