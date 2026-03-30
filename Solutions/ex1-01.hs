main = do
    print "Testando 'maior4'"
    print ( teste 1 4 3 2 4 ) 
    print ( teste 15 7 0 3 15 ) 
    print ( teste 2 1 1 0 2 ) 
    print ( teste 3 3 3 3 3 )

-- Resolução do exercício
maior :: Int -> Int -> Int
maior x y | x >= y    = x
          | otherwise = y

maior4 :: Int -> Int -> Int -> Int -> Int
maior4 x y z w = maior (maior x y) (maior z w)


-- Funções de teste
testeCaso x y z w esp = maior4 x y z w == esp

teste x y z w esp | testeCaso x y z w esp = "PASSOU"
                  | otherwise             = "FALHA: esp " ++ show esp ++ " obtido " ++ show (maior4 x y z w)