module Main (main) where
import Control.Concurrent.STM (STM)
import Original.Crazy (Row(i))
import Control.Concurrent
import Control.Concurrent.STM.TBQueue
import Control.Monad.STM
import Control.Monad

main :: IO ()
main = do 
  queue <- newTBQueueIO 10_000
  void . forkIO $ one queue
  void . forkIO $ two queue

  atomically $ writeTBQueue queue ""
  aux queue
  pure ()

aux :: TBQueue String -> IO ()
aux queue = do
  s <- atomically $ readTBQueue queue
  print s
  atomically $ writeTBQueue queue s
  threadDelay 10_000
  aux queue

one :: TBQueue String -> IO ()
one queue = do 
  s <- atomically $ readTBQueue queue
  print s
  atomically $ writeTBQueue queue (s ++ " - one")
  threadDelay 1000
  one queue

two :: TBQueue String -> IO ()
two queue = do 
  s <- atomically $ readTBQueue queue
  print s
  atomically $ writeTBQueue queue (s ++ " - two")
  threadDelay 1000
  two queue
  
  
