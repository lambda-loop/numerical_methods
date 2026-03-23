{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Original.Crazy where

-- In this first version, we dont have a server for everyone, so.. Its crazy!!
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import GHC.Arr
import qualified Data.Vector.Storable as V
import Numeric.LinearAlgebra
import Control.Monad.ST
import Data.Foldable.WithIndex
import GaussSeidel.Serial (initialSolution)

import Numeric.LinearAlgebra
import Data.Traversable.WithIndex (ifor)
import Control.Monad
import Control.Exception.Base (nestedAtomically)
import Control.Concurrent (forkIO, threadDelay)

-- TODO: lenses when
data Row = Row
  { aᵢᵢ :: Double
  , bᵢ  :: Double
  , i   :: Int
  
  ---- they could be together somehow?
  , xs :: Vector Double -- replace???
  , as :: Vector Double -- replace???

  , receive   :: TBQueue (Int, Double)
  , send      :: TBQueue (Int, Double)
  } 

mkRow :: Vector Double 
      -> Int 
      -> Vector Double 
      -> TBQueue (Int, Double)
      -> TBQueue (Int, Double)
      -> Row
mkRow v i xs r s = 
  let aii = v `atIndex` i
      bi  = v `atIndex` size v - 1
      as  = dropIndex i v
  in Row {
    aᵢᵢ=aii,
    bᵢ = bi,
    i  = i,
 
    xs = xs,
    as = as,

    receive = r,
    send    = s
  }
  

instance Show Row where
  show Row {..} = 
    "Row { a"   ++ show i  ++ show i  ++ ": " ++ show aᵢᵢ
    ++ ", b"    ++ show i  ++ ": "    ++ show bᵢ 
    ++ ", xs: " ++ show xs ++ ", as:" ++ show as 
    ++ ", i: "  ++ show i  ++ "} "
  

-- BUG: can access the own indice
-- shared queue (total mess)
initializeLines :: TBQueue (Int, Double) -> TBQueue (Int, Double) -> Matrix Double -> [Row]
initializeLines send receive m = runST do 
  let last_col = cols m - 1
      js       = [0..last_col]
      s = initialSolution m

  ifor (toRows m) \i r -> do 
    let aᵢᵢ = 1 / r V.! i
        -- slow
        bᵢ  = r V.! last_col
        as  = negate . (r V.!) <$> filter (/=i) js 
    pure Row {
          aᵢᵢ = aᵢᵢ,
          bᵢ  = bᵢ,
          send    = send,
          receive = receive,

          xs = V.ifilter (\i' _ -> i' /= i) s,
          as = V.fromList as,

          i = i
    }

start :: Matrix Double -> IO ()
start m = do
  let rs = toRows m
  undefined

monitor :: TBQueue (Int, Double) -> Vector Double -> IO ()
monitor q s = do
  threadDelay 5_000
  us <- atomically do 
    h <- readTBQueue q
    t <- flushTBQueue q
    pure (h:t)
  let s' = V.unsafeUpd s us
  print s' 
  monitor q s'

server :: TBQueue (Int, Double)             -- receive
       -> Array Int (TBQueue (Int, Double)) -- send
       -> IO ()
server r ss = do
  us <- atomically do 
    h <- readTBQueue r
    t <- flushTBQueue r
    pure (h:t)
  
  -- concurrently? nested concurently? 
  -- big atomically or a lot of small ones?
  forM_ us \(i, x) -> do
    forkIO do
      iforM_ ss \j s -> 
        when (j /= i) do
          atomically $ writeTBQueue s (i, x)

  server r ss
  

worker :: Row -> IO ()
worker r = do
  us <- atomically do 
    h <- readTBQueue (receive r)
    t <- flushTBQueue (receive r)
    pure (h:t)

  let adjust j | j < i r = j 
               | otherwise = j - 1
      us' = (\(j, x) -> (adjust j, x)) <$> us
      xs' = V.unsafeUpd (xs r) us'
      x' = (aᵢᵢ r *) $ 
        bᵢ r + V.sum (V.zipWith (*) (as r) xs')

  atomically $ writeTBQueue (send r) (i r, x')
  worker r 
  

v1 :: Matrix Double
v1 =  (4><4) [1..]

v2 :: Matrix Double
v2 =  (4><4) [10..]

v3 :: Matrix Double
v3 = (3><4) 
  [10, 2, 1, 7
  ,1, 5, 1, (-8)
  ,2, 3, 10, 6]
    
dropIndex :: Int -> Vector Double -> Vector Double
dropIndex i v = vjoin [subVector 0 i v, subVector (i+1) 
  ((size v) - i - 1) v]



  

  
