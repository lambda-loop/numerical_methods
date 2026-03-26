module Main (main) where
import Control.Concurrent.STM (STM)
import Original.Crazy (Row(i))
import Control.Concurrent
import Control.Concurrent.STM.TBQueue
import Control.Monad.STM
import Control.Monad
import Original.Amortization

main :: IO ()
main = do
  print "Running"
  testao 1000

