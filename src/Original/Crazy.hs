
module Original.Crazy where
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import GHC.Arr
import qualified Data.Vector.Storable as V

data Line = Line 
  { aii :: Double
  , bi  :: Double
  , xii :: TVar Double
  -- they could be together somehow?
  , xs :: TArray Int Double -- replace?
  , as :: Array Int Double  -- replace?
  }
