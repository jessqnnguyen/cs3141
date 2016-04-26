module IORef where

import Data.IORef

printIORef :: IORef Int -> IO ()
printIORef ref
  = do
      v <- readIORef ref
      print v

main
  = do
      ref <- newIORef 10
      printIORef ref
      writeIORef ref 42
      printIORef ref