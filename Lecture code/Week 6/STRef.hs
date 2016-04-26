module STRef where
  
import Data.STRef
import Control.Monad.ST

useState
  = runST $ do
      ref <- newSTRef 0
      writeSTRef ref 42
      v <- readSTRef ref
      return v
      

main = print useState 
