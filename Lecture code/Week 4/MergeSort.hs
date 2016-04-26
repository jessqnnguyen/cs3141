module MergeSort where
{-# LANGUAGE ScopedType Variables #-} 

import Data.List
import Test.QuickCheck

-- merge recursively merges two arrays
merge :: Ord a => [a] -> [a] -> [a]
-- base case 1: list 1 is empty
merge []      ys    = ys
-- base case 2: list 2 is empty
merge xs      []    = xs
-- note that there is no need to check for the both lists are empty case since it should be caught by the first two base cases
--merge []      []    = []
merge (x:xs) (y:ys)
  -- 2 recursive cases 
  | x <= y          = x:merge xs (y:ys)
  | otherwise      = y:merge (x:xs) ys 

-- split recursively splits an array
split :: [a] -> ([a], [a])
-- base case 1: empty list
split []        = ([], [])
-- recursive case 1: if there is 1 element split the array into 1 array containing the element and the second array an empty list
split [element] = ([element], [])
-- recursive case 2: when there 2 or more elements in the list
split (element1:(element2:rest))
  = (element1:rest1, element2:rest2)
  where 
    (rest1, rest2) = split rest

mergesort :: Ord a => [a] -> [a]
-- base case 1: list is empty
mergesort [] = [] 
-- base case 2: list only contains 1 element
mergesort [x] = [x]
-- recursive case
mergesort xs = merge result1 result2
  where
    (list1, list2) = split xs
    result1        = mergesort list1
    result2        = mergesort list2

prop_preservesLength :: [Int] -> Bool
prop_preservesLength xs = 
  length xs == length (mergesort xs)

-- check it is idempotent, a function applied twice will output the same thing
prop_idempotent :: [Int] -> Bool
prop_idempotent xs =
  mergesort xs == mergesort (mergesort xs)
  
prop_ordered :: [Int] -> Bool
prop_ordered xs =
  isSorted (mergesort xs)

-- look at the first two elements only and compare then drop one of those two elements we just compared  and compare with the rest of the list recursively
isSorted :: Ord a => [a] -> Bool
-- base case 1: empty list
isSorted []       = True
-- base case 2: list of length 1
isSorted [x]      = True
-- recursive case
isSorted (x:y:ys)
  | x <= y = isSorted (y:ys)
  | otherwise = False
  
prop_permutation :: [Int] -> Bool
prop_permutation xs = 
  isPermutation (mergesort xs) xs
  where -- null checks for an empty list
     isPermutation xs ys = null (xs \\ ys) &&
                           null (ys \\ xs) 
                           

-- helper
quickCheck' prop
  = output <$>
    quickCheckWithResult stdArgs{chatty = False} prop 