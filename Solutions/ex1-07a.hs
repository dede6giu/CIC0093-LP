main = do
    print "Testando 'somaMatricial'"
    print ( teste [[1,2,3,4], [ 3,0, 4, 1], [ 5, 9, -2,0]]
                  [[4,3,2,1], [-3,0, 7,-2], [-3, 4, -8,1]]
                  [[5,5,5,5], [ 0,0,11,-1], [ 2,13,-10,1]] )

-- Resolução do exercício
somaMatricial :: [[Int]] -> [[Int]] -> [[Int]]
somaMatricial [] _ = []
somaMatricial _ [] = []
somaMatricial (x:xs) (y:ys) = somaLinha x y : somaMatricial xs ys 

somaLinha [] _ = []
somaLinha _ [] = []
somaLinha (x:xs) (y:ys) = (x+y) : somaLinha xs ys 

-- Funções de teste
testeCaso x y esp = somaMatricial x y == esp

teste x y esp | testeCaso x y esp = "PASSOU"
              | otherwise         = "FALHA: esp " ++ show esp ++ " obtido " ++ show (somaMatricial x y)