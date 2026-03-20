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

data Row = Row
  { aᵢᵢ :: Double
  , bᵢ  :: Double
  
  ---- they could be together somehow?
  , xs :: Vector Double -- replace???
  , as :: Vector Double -- replace???

  , q   :: TBQueue (Int, Double)
  , i   :: Int
  } 

instance Show Row where
  show Row {..} = 
    "Row { a"   ++ show i  ++ show i ++ ": " ++ show aᵢᵢ
    ++ ", b"    ++ show i  ++ ": "   ++ show bᵢ 
    ++ ", xs: " ++ show xs ++ ", as:" ++ show as 
    ++ ", i: "  ++ show i  ++ "} "
  

-- BUG: can access the own indice
-- shared queue (total mess)
initializeLines :: TBQueue (Int, Double) -> Matrix Double -> [Row]
initializeLines q m = runST do 
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
          q   = q,

          xs = V.ifilter (\i' _ -> i' /= i) s,
          as = V.fromList as,

          i = i
    }

-- calc and send
-- TODO: Try with the queueB list version!
worker :: Row -> STM ()
worker r = do
  (j, x) <- readTBQueue (q r)
  when (j == i r) retry

  let j' = if j < i r then j else j - 1
      xs' = V.unsafeUpd (xs r) [(j', x)]
      x' = (aᵢᵢ r *) $ 
        bᵢ r + V.sum (V.zipWith (*) (as r) xs')

  writeTBQueue (q r) (i r, x')
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
    




  

  
