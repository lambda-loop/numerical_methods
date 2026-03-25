
module Read where

import Numeric.LinearAlgebra
import Data.List (isPrefixOf)

parseMatrixFloat :: String -> Double
parseMatrixFloat s = read (fixFloat s)
  where
    -- Coloca o zero à esquerda se começar com ponto ou menos-ponto
    fixFloat ('-':'.':rest) = "-0." ++ replaceD rest
    fixFloat ('.':rest)     = "0." ++ replaceD rest
    fixFloat str            = replaceD str
    
    -- Troca o 'D' ou 'd' da notação científica antiga por 'e'
    replaceD = map (\c -> if c == 'D' || c == 'd' then 'e' else c)

-- | Lê um arquivo .mtx simétrico e retorna a tupla (Matriz A, Vetor b)
loadSystem :: FilePath -> IO (Matrix Double, Vector Double)
loadSystem filepath = do
    contents <- readFile filepath
    
    let ls = lines contents
        -- 1. Remove os comentários e ignora linhas totalmente em branco
        dataLines = filter (\l -> not ("%" `isPrefixOf` l) && not (null (words l))) ls
        
        header = head dataLines
        [nRows, nCols, _] = map read (words header) :: [Int]
        
        -- 2. Parser com a nossa "vacina" aplicada no valor
        parseTuples :: String -> [((Int, Int), Double)]
        parseTuples line = 
            let [rStr, cStr, vStr] = words line
                r = read rStr - 1
                c = read cStr - 1
                v = parseMatrixFloat vStr  -- <- Usando o parser corrigido aqui!
            in if r == c 
               then [((r, c), v)] 
               else [((r, c), v), ((c, r), v)]
        
        tuples = concatMap parseTuples (tail dataLines)
        
        matrixA = assoc (nRows, nCols) 0.0 tuples
        xReal = konst 1.0 nRows
        vectorB = matrixA #> xReal
        
    -- Força a avaliação estrita da matriz para garantir que erros de leitura 
    -- aconteçam AQUI e não lá na frente durante a impressão.
    matrixA `seq` vectorB `seq` return (matrixA, vectorB)

-- Exemplo de como usar no seu programa principal:
test :: IO ()
test = do
    putStrLn "Carregando a matriz..."
    (a, b) <- loadSystem "ms/1138_bus.mtx"    

    putStrLn $ "Matriz A carregada com dimensões: " ++ show (rows a) ++ "x" ++ show (cols a)
    putStrLn $ "Vetor b gerado com tamanho: " ++ show (size b)

    -- print a
    print (a `atIndex` (15, 15))
    print (a `atIndex` (15, 16))
    print (a `atIndex` (14, 14))
    print (a `atIndex` (14, 13))
    print (a `atIndex` (13, 13))
    print (a `atIndex` (13, 17))
    -- print b
    
    -- Aqui você chamaria o seu algoritmo:
    -- let x_calculado = meuGaussSeidel a b
    -- print x_calculado

createAugmentedMatrix :: Matrix Double -> Vector Double -> Matrix Double
createAugmentedMatrix a b = a ||| asColumn b
