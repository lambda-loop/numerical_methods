{-# LANGUAGE BlockArguments #-}

module Original.Amortization where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.STRef
import GHC.IO (unsafePerformIO)
import Control.Concurrent.Async
import Read

-- TODO: have the rows in instant time omg? carry the sizes..?
initialSolution :: Matrix Double -> Vector Double
initialSolution m = runST do
  let m_rows   = rows m
  let last_col = cols m - 1
  
  solution <- VM.unsafeNew m_rows

  forM_ [0..m_rows-1] \i -> do
    let aᵢᵢ = m `atIndex` (i, i)
        bᵢ  = m `atIndex` (i, last_col)
    VM.write solution i (bᵢ/aᵢᵢ)

  V.unsafeFreeze solution

-- badConcurrency
next' :: Matrix Double -> Vector Double -> IO (Vector Double)
next' m solution = do
  let m_rows   = rows m
  let last_col = cols m - 1

  solution' <- VM.unsafeNew (size solution)

  forConcurrently_ [0..m_rows - 1] \i -> do
    let aᵢᵢ= m `atIndex` (i, i)
        bᵢ = m `atIndex` (i, last_col)
        -- slow
        axs = do 
          j <- filter (/=i) [0..rows m-1]
          let a = m `atIndex` (i, j)
              x = solution V.! j
          pure $ -(a*x)
        vals = sum axs + bᵢ 

    VM.write solution' i (vals/aᵢᵢ)

  V.unsafeFreeze solution'
  

v1 :: Matrix Double
v1 =  (4><4) [1..]

v2 :: Matrix Double
v2 =  (4><4) [10..]

v3 :: Matrix Double
v3 = (3><4) 
  [10, 2, 1, 7
  ,1, 5, 1, (-8)
  ,2, 3, 10, 6]

-- not concurrent
next :: Matrix Double -> Vector Double -> IO (Vector Double)
next m solution = do
  let m_rows   = rows m
  let last_col = cols m - 1

  solution' <- VM.unsafeNew (size solution)

  forM_ [0..m_rows - 1] \i -> do
    let aᵢᵢ= m `atIndex` (i, i)
        bᵢ = m `atIndex` (i, last_col)
        -- slow
        axs = do 
          j <- filter (/=i) [0..rows m-1]
          let a = m `atIndex` (i, j)
              x = solution V.! j
          pure $ -(a*x)
        vals = sum axs + bᵢ 

    VM.write solution' i (vals/aᵢᵢ)

  V.unsafeFreeze solution'

stopCond :: Vector Double -> Vector Double -> Double -> Bool
stopCond xs xs' epsi = 
  let diff = cmap abs (xs - xs')
      normDiff = maxElement diff
      normXs' = maxElement (cmap abs xs')
  in (normDiff / normXs') < epsi

run' :: Matrix Double -> IO (Vector Double)
run' m = do
  let xs = initialSolution m 
  xs' <- next' m xs
  loop xs xs'
  where 
    loop xs xs' | stopCond xs xs' epsi = pure xs'
                | otherwise = do
        xs'' <- next' m xs'
        -- print xs''
        loop xs' xs''

run'' :: Int -> Matrix Double -> IO (Vector Double)
run'' n m = do
  let xs = initialSolution m 
  xs' <- next' m xs
  loop n xs xs'
  where 
    loop 0 xs xs' | stopCond xs xs' epsi = pure xs'
                  | otherwise = do
        xs'' <- next' m xs'
        loop n xs' xs''
    loop n' _ xs' = do 
      xs'' <- next' m xs'
      loop (n'-1) xs' xs''
      

mGen :: String -> IO (Matrix Double)
mGen filepath = do
  (m, b) <- loadSystem filepath
  pure (createAugmentedMatrix m b)

clock ax = do
  start <- getCurrentTime
  x <- ax
  end <- getCurrentTime 
  pure (diffUTCTime end start, x)

-- mTest = mGen "ms/new/153.mtx"    CULPADA
-- mTest   = mGen "ms/new/147.mtx"  CULPADA
-- mTest   = mGen "ms/new/48.mtx"      INOCENTE
-- mTest = mGen "ms/new/124.mtx"    CULPADA
-- mTest = mGen "ms/new/66.mtx"     CULPADA 
-- mTest = mGen "ms/new/27.mtx"     CULPADA
-- mTest = mGen "ms/new/4K.mtx"     
-- mTest = gerarMatrizJacobi 10_000

testao n = do
  !m       <- gerarMatrizJacobi n
  (t , _) <- clock (run' m)
  (t0, _) <- clock (run'' 0 m)
  (t1, _) <- clock (run'' 1 m)
  (t2, _) <- clock (run'' 2 m)
  (t3, _) <- clock (run'' 3 m)
  (t4, _) <- clock (run'' 4 m)
  (t5, _) <- clock (run'' 5 m)
  (t6, _) <- clock (run'' 6 m)
  (t7, _) <- clock (run'' 7 m)
  (t8, _) <- clock (run'' 8 m)
  (t9, _) <- clock (run'' 9 m)
  (t10, _) <- clock (run'' 10 m)
  print t
  print t0
  print t1
  print t2
  print t3
  print t4
  print t5
  print t6
  print t7
  print t8
  print t9
  print t10

m4K    = mGen "ms/new/4K.mtx"
test4K = do
  m <- m4K
  -- (t0, _) <- clock (run'' 50000 m)
  -- (t1, _) <- clock (run'' 5000 m)
  -- (t2, _) <- clock (run'' 500 m)
  (t3, _) <- clock (run' m)
  -- print t0
  -- print t1
  -- print t2
  print t3
  
m10K    = mGen "ms/new/10K.mtx"
test10K = do
  m <- m10K
  clock (run'' 50000 m)
  clock (run'' 5000 m)
  clock (run'' 500 m)
  clock (run' m)

m20K  = mGen "ms/new/20K.mtx"
test20K = do
  m <- m20K
  clock (run'' 50000 m)
  clock (run'' 5000 m)
  clock (run'' 500 m)
  clock (run' m)

m200K = mGen "ms/new/200K.mtx"
test200K = do
  m <- m200K
  clock (run'' 50000 m)
  clock (run'' 5000 m)
  clock (run'' 500 m)
  clock (run' m)

-- AI AI AI AI
-- Função Detetive: Verifica se a matriz atende aos requisitos do Jacobi
detetive :: Matrix Double -> IO ()
detetive m = do
  let n = rows m
      -- 1. Separa só a matriz A (ignorando a última coluna que é o vetor B)
      matA = takeColumns n m
      linhas = toLists matA -- Converte pra lista do Haskell pra facilitar a inspeção
  
  putStrLn "🔍 Iniciando a investigação da matriz..."
  
  -- 2. Varre as linhas procurando os crimes (Diagonal fraca)
  let crimes = do 
        (i, linha) <- zip [0..] linhas
        let a_ii = abs (linha !! i)
            somaVizinhos = sum (map abs linha) - a_ii
        -- O Crime: A diagonal é menor ou igual à soma dos vizinhos?
        if a_ii <= somaVizinhos
          then [(i, a_ii, somaVizinhos)]
          else []
          
  -- 3. Emite o veredito
  if null crimes
    then putStrLn "✅ Matriz INOCENTE! A diagonal é dominante. O Jacobi DEVE funcionar."
    else do
      putStrLn $ "🚨 Matriz CULPADA! Encontramos " ++ show (length crimes) ++ " linha(s) onde a diagonal perde pros vizinhos."
      putStrLn "Aqui estão as 5 primeiras infrações (Linha | Valor Absoluto Diagonal | Soma dos Vizinhos):"
      forM_ (take 5 crimes) \(i, diag, soma) ->
        putStrLn $ " -> Linha " ++ show i ++ ": Diagonal = " ++ show diag ++ " | Soma = " ++ show soma

-- Injeta esteroides na diagonal para forçar o Jacobi a funcionar
domarMatriz :: Matrix Double -> Matrix Double
domarMatriz m = 
  let n = rows m
      matA = takeColumns n m
      vecB = dropColumns n m -- Salva o vetor b intacto
      
      -- Cria uma matriz só com números 10000 na diagonal
      esteroides = diag (konst 10000 n)
      
      -- Soma os esteroides na matriz A original
      matA_domada = matA + esteroides
      
  -- Junta o novo A com o B de volta usando o operador ||| (concatenação horizontal)
  in matA_domada ||| vecB

-- O Laboratório: Gera uma matriz de tamanho N perfeitamente dominada na diagonal
gerarMatrizJacobi :: Int -> IO (Matrix Double)
gerarMatrizJacobi n = do
  putStrLn $ "🧪 Fabricando uma matriz " ++ show n ++ "x" ++ show n ++ " à prova de NaN..."
  
  -- 1. Gera uma matriz NxN com números aleatórios (entre 0 e 1, multiplicados por 10)
  matRaw <- rand n n
  let matAleatoria = scale 10.0 matRaw
      
  -- Vamos converter para as listas clássicas do Haskell para manipular a diagonal
  let linhas = toLists matAleatoria
  
  -- 2. A MÁGICA: Reconstrói as linhas garantindo a Dominância Diagonal
  let linhasDominantes = do
        (i, linha) <- zip [0..] linhas
        
        -- Soma todo mundo, mas tira o cara que já tá na diagonal
        let somaVizinhos = sum (map abs linha) - abs (linha !! i)
        
        -- A nova diagonal vai ser a soma de todos os vizinhos + 5.0 (o "chorinho" pra garantir a vitória)
        let novaDiagonal = somaVizinhos + 5.0
        
        -- Coloca o peso pesado na cadeira da diagonal (posição 'i')
        let novaLinha = take i linha ++ [novaDiagonal] ++ drop (i + 1) linha
        return novaLinha
        
  -- 3. Transforma as listas de volta para o formato de alta performance da hmatrix
  let !matA_perfeita = fromLists linhasDominantes
  
  -- 4. Gera o vetor B aleatório também (Nx1)
  vecB_raw <- rand n 1
  let !vecB = scale 10.0 vecB_raw
  
  -- 5. Cola o vetor B do lado direito da matriz A (Matriz Aumentada) usando o operador |||
  pure (matA_perfeita ||| vecB)

epsi = 1e-15
