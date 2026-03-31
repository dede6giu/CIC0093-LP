main = do
    print "Testando 'myZipWith'"
    print ( teste somaDobroB [1,2,1,5,3] [0,2,4,6,4] [1,6,9,17,11] )
    print ( teste imparQueijo [12,65,69,34] ["manteiga", "farinha", "queijo", "queijo"] [False, False, True, False] )

-- Resolução do exercício
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = [f a b] ++ myZipWith f as bs


-- Funções de teste
testeCaso f a b esp = myZipWith f a b == esp

teste f a b esp | testeCaso f a b esp = "PASSOU"
                | otherwise           = "FALHA: esp " ++ show esp ++ " obtido " ++ show (myZipWith f a b)

somaDobroB a b = a + 2*b
imparQueijo a b = (mod a 2 == 1) && (b == "queijo")