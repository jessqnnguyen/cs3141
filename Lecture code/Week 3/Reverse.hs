module Reverse where

import Prelude hiding (reverse)
import Test.QuickCheck

reverse :: [a] -> [a]
reverse []  = []
reverse (x : xs) = reverse xs ++ [x]

prop_revApp :: [String] -> [String] -> Bool
prop_revApp xs ys =
  reverse (xs ++ ys) == reverse ys 
  ++ reverse xs
  
fastReverse :: [a] -> [a]
fastReverse xs = rev [] xs
  where
    rev rxs []    = rxs
    rev rxs (x:xs) = rev (x:rxs) xs

prop_fastRev xs = 
  reverse xs == fastReverse xs

{- Helper test function which just outputs
 - quickCheck's output to the side instead in the 
 - bottom panel
 -}
quickCheck' prop
   = output <$>
      quickCheckWithResult stdArgs{chatty = False} prop