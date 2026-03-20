{-# LANGUAGE BlockArguments #-}

module GaussJacobi.Serial where

import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.STRef
import GHC.IO (unsafePerformIO)

data LinearSystem = LS 

debug :: Show a => a -> a
debug x = unsafePerformIO do
  print x
  pure x

while :: (a -> Bool) -> ST s a -> ST s a 
while p st = do 
  x <- st
  if p x then while p st
  else pure x

while_ :: (a -> Bool) -> ST s a -> ST s ()
while_ p st = do 
  x <- st
  if p x then while_ p st
  else pure ()

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

next :: Matrix Double -> Vector Double -> Vector Double
next m solution = runST do
  let m_rows   = rows m
  let last_col = cols m - 1

  solution' <- VM.unsafeNew (size solution)

  forM_ [0..rows m-1] \i -> do
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



