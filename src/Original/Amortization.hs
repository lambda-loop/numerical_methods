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

testao n = do
  !m       <- generateMatrix n
  (t ,  !_) <- clock (run' m)
  (t0,  !_) <- clock (run'' 0 m)
  (t1,  !_) <- clock (run'' 1 m)
  (t2,  !_) <- clock (run'' 2 m)
  (t3,  !_) <- clock (run'' 3 m)
  (t4,  !_) <- clock (run'' 4 m)
  (t5,  !_) <- clock (run'' 5 m)
  (t6,  !_) <- clock (run'' 6 m)
  (t7,  !_) <- clock (run'' 7 m)
  (t8,  !_) <- clock (run'' 8 m)
  (t9,  !_) <- clock (run'' 9 m)
  (t10, !_) <- clock (run'' 10 m)
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

generateMatrix :: Int -> IO (Matrix Double)
generateMatrix n = do
  print "generating"
  matRaw <- rand n n
  let randMat = scale 10.0 matRaw
      
  let rows_ = toLists randMat
  
  let dominantRows = do
        (i, row_) <- zip [0..] rows_
        let neighborhood_sum = sum (map abs row_) - abs (row_ !! i)
        -- let higher = maximum (map abs row_) -- - abs (row_ !! i)
        
        let newDiag = neighborhood_sum + 0.25
        
        let newRow = take i row_ ++ [newDiag] ++ drop (i + 1) row_
        return newRow
        
  let !perfectM = fromLists dominantRows
  
  vecB_raw <- rand n 1
  let !vecB = scale 10.0 vecB_raw
  
  print "end generation"
  pure (perfectM ||| vecB)

epsi :: Double
epsi = 1e-5
