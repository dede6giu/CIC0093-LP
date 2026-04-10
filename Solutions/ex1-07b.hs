main = do
    print "Testando 'matrizTransposta'"
    print ( teste [[1,2,3,4], [3,0,4,1], [5,9,-2,0]]
                  [[1,3,5], [2,0,9], [3,4,-2], [4,1,0]] )
    print ( teste [[1,2], [3,4]] [[1,3], [2,4]] )
    print ( teste [[0]] [[0]] )
    print ( teste [[]] [] )
    print ( teste [] [] )

-- Resolução do exercício
matrizTransposta :: [[Int]] -> [[Int]]
matrizTransposta [] = []
matrizTransposta (x:xs) = colToLin (tamanho x - 1) (x:xs)

colToLin n _ | n < 0 = []
colToLin n x = (colToLin (n-1) x) ++ [cadaListaElem n x]

tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

cadaListaElem _ [] = []
cadaListaElem n (x:xs) = elemLista n x : cadaListaElem n xs

elemLista _ [x] = x
elemLista n (x:xs) | n == 0 = x
                   | n >= 1 = elemLista (n-1) xs

-- Funções de teste
testeCaso x esp = matrizTransposta x == esp

teste x esp | testeCaso x esp = "PASSOU"
            | otherwise       = "FALHA: esp " ++ show esp ++ " obtido " ++ show (matrizTransposta x)