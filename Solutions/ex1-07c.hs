main = do
    print "Testando 'multiplicacaoMatricial'"
    print ( teste [[1,2],
                   [3,4]]
                  [[5,6],
                   [7,8]]
                  [[19,22],
                   [43,50]] )
    print ( teste [[1,1,1],
                   [2,2,2]]
                  [[1,2],
                   [2,3],
                   [3,4]]
                  [[6,9],
                   [12,18]]  )
    print ( teste [] [] [] )
    print ( teste [[0]] [[1,2]] [[0,0]])

-- Resolução do exercício
multiplicacaoMatricial :: [[Int]] -> [[Int]] -> [[Int]]
multiplicacaoMatricial x y = multMatriz x (matrizTransposta y)

multMatriz [] _ = []
multMatriz (x:xs) y = multLinha x y : multMatriz xs y

multLinha _ [] = []
multLinha x (y:ys) = somaMultLista x y : multLinha x ys

somaMultLista [] [] = 0
somaMultLista (x:xs) (y:ys) = x*y + somaMultLista xs ys

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
testeCaso x y esp = multiplicacaoMatricial x y == esp

teste x y esp | testeCaso x y esp = "PASSOU"
              | otherwise         = "FALHA: esp " ++ show esp ++ " obtido " ++ show (multiplicacaoMatricial x y)