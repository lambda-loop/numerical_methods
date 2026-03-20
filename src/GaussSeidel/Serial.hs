{-# LANGUAGE BlockArguments #-}
module GaussSeidel.Serial where

import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.STRef
import GHC.IO (unsafePerformIO)

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

  solution' <- V.unsafeThaw solution
  -- solution' <- VM.unsafeNew (size solution)

  forM_ (reverse [0..m_rows-1]) \i -> do
  -- forM_ [0..m_rows-1] \i -> do
    -- soo slow
    let other_is = filter (/=i) [0..rows m-1]
    xs <- forM other_is (solution' `VM.read`) 
    let aᵢᵢ= m `atIndex` (i, i)
        bᵢ = m `atIndex` (i, last_col)

        -- slow
        axs = zipWith (\j x -> -(m `atIndex` (i, j))*x) other_is xs
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


v3' = initialSolution v3
next' = next v3 


next0 :: Matrix Double -> Vector Double -> Vector Double
next0 m solution = runST do
  let m_rows   = rows m
  let last_col = cols m - 1

  solution' <- V.unsafeThaw solution
  -- solution' <- VM.unsafeNew (size solution)

  forM_ (reverse (filter (/=0) [0..m_rows-1])) \i -> do
  -- forM_ [0..m_rows-1] \i -> do
    -- soo slow
    let other_is = filter (/=i) [0..rows m-1]
    xs <- forM other_is (solution' `VM.read`) 
    let aᵢᵢ= m `atIndex` (i, i)
        bᵢ = m `atIndex` (i, last_col)

        -- slow
        axs = zipWith (\j x -> -(m `atIndex` (i, j))*x) other_is xs
        vals = sum axs + bᵢ 

    VM.write solution' i (vals/aᵢᵢ)
  V.unsafeFreeze solution'


next1 :: Matrix Double -> Vector Double -> Vector Double
next1 m solution = runST do
  let m_rows   = rows m
  let last_col = cols m - 1

  solution' <- V.unsafeThaw solution
  -- solution' <- VM.unsafeNew (size solution)

  forM_ (reverse (filter (/=1) [0..m_rows-1])) \i -> do
  -- forM_ [0..m_rows-1] \i -> do
    -- soo slow
    let other_is = filter (/=i) [0..rows m-1]
    xs <- forM other_is (solution' `VM.read`) 
    let aᵢᵢ= m `atIndex` (i, i)
        bᵢ = m `atIndex` (i, last_col)

        -- slow
        axs = zipWith (\j x -> -(m `atIndex` (i, j))*x) other_is xs
        vals = sum axs + bᵢ 

    VM.write solution' i (vals/aᵢᵢ)
  V.unsafeFreeze solution'


next2 :: Matrix Double -> Vector Double -> Vector Double
next2 m solution = runST do
  let m_rows   = rows m
  let last_col = cols m - 1

  solution' <- V.unsafeThaw solution
  -- solution' <- VM.unsafeNew (size solution)

  forM_ (reverse (filter (/=2) [0..m_rows-1])) \i -> do
  -- forM_ [0..m_rows-1] \i -> do
    -- soo slow
    let other_is = filter (/=i) [0..rows m-1]
    xs <- forM other_is (solution' `VM.read`) 
    let aᵢᵢ= m `atIndex` (i, i)
        bᵢ = m `atIndex` (i, last_col)

        -- slow
        axs = zipWith (\j x -> -(m `atIndex` (i, j))*x) other_is xs
        vals = sum axs + bᵢ 

    VM.write solution' i (vals/aᵢᵢ)
  V.unsafeFreeze solution'

test v = 
  let v' = initialSolution v
  in 
  ( next0 v . next0 v . next2 v . next1 v . next2 v . next0 v
  . next1 v . next2 v . next0 v . next2 v . next1 v . next1 v
  . next2 v . next1 v . next0 v . next0 v . next0 v . next1 v)
   $ next v v'

  
