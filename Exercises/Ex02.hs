module Ex02 (
  BinaryTree(..), isBST, insert, deleteAll, searchTrees, isSorted, sortedListsWithoutDuplicates, isBalanced,
  mysteryPred, mysterious, astonishing
) where

import Test.QuickCheck
import Data.List(sort, nub)

data BinaryTree = Branch Integer BinaryTree BinaryTree
                | Leaf
                deriving (Show, Ord, Eq)

isBST :: BinaryTree -> Bool
isBST Leaf = True
isBST (Branch v l r) = allTree (< v) l && allTree (>= v) r && isBST l && isBST r
  where allTree :: (Integer -> Bool) -> BinaryTree -> Bool
        allTree f (Branch v l r) = f v && allTree f l && allTree f r
        allTree f (Leaf) = True
        
        
--Add an integer to a BinaryTree, preserving BST property.
insert :: Integer -> BinaryTree -> BinaryTree
insert i Leaf = Branch i Leaf Leaf
insert i (Branch v l r) 
  | i < v = Branch v (insert i l) r
  | otherwise = Branch v l (insert i r)

--Remove all instances of an integer in a binary tree, preserving BST property
deleteAll :: Integer -> BinaryTree -> BinaryTree
deleteAll i Leaf = Leaf
deleteAll i (Branch j Leaf r) | i == j = deleteAll i r
deleteAll i (Branch j l Leaf) | i == j = deleteAll i l
deleteAll i (Branch j l r) | i == j = let (x, l') = deleteRightmost l
                                       in Branch x l' (deleteAll i r)
                           | i <  j = Branch j (deleteAll i l) r
                           | i >  j = Branch j l (deleteAll i r)
  where deleteRightmost :: BinaryTree -> (Integer, BinaryTree)
        deleteRightmost (Branch i l Leaf) = (i, l)
        deleteRightmost (Branch i l r)    = let (x, r') = deleteRightmost r
                                             in (x, Branch i l r')
                                           
prop_searchTrees = forAll searchTrees isBST

prop_mysteryPred_1 integer = forAll searchTrees $ \tree -> mysteryPred integer (insert integer tree)

prop_mysteryPred_2 integer = forAll searchTrees $ \tree -> not (mysteryPred integer (deleteAll integer tree))

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where 
   searchTrees' 0 = return Leaf
   searchTrees' n = do 
      v <- (arbitrary :: Gen Integer)
      fmap (insert v) (searchTrees' $ n - 1)


----Add an integer to a BinaryTree, preserving BST property.
--insert :: Integer -> BinaryTree -> BinaryTree
--insert i Leaf = Branch i Leaf Leaf
--insert i (Branch v l r) 
--  | i < v = Branch v (insert i l) r
--  | otherwise = Branch v l (insert i r)
----------------------

mysteryPred :: Integer -> BinaryTree -> Bool
mysteryPred x Leaf = False
mysteryPred x (Branch v l r)
  | v == x = True
  | x < v = mysteryPred x l
  | x > v = mysteryPred x r


----------------------

mysterious :: BinaryTree -> [Integer]
mysterious Leaf = []
mysterious (Branch v l r) = sort $ v : mysterious l ++ mysterious r

prop_mysterious_1 integer = forAll searchTrees $ \tree -> mysteryPred integer tree == (integer `elem` mysterious tree)

prop_mysterious_2 = forAll searchTrees $ isSorted . mysterious

isSorted :: [Integer] -> Bool
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
isSorted _ = True

----------------------

-- Note `nub` is a function that removes duplicates from a sorted list
sortedListsWithoutDuplicates :: Gen [Integer]
sortedListsWithoutDuplicates = fmap (nub . sort) arbitrary

prop_sortedListsWithoutDuplicates_1 = forAll sortedListsWithoutDuplicates isSorted 
prop_sortedListsWithoutDuplicates_2 = forAll sortedListsWithoutDuplicates $ \x -> x == nub x

astonishing :: [Integer] -> BinaryTree
astonishing [] = Leaf
astonishing [x] = insert x Leaf
astonishing (x:xs) = insert x (astonishing xs) 
 
prop_astonishing_1 = forAll sortedListsWithoutDuplicates $ isBST . astonishing

prop_astonishing_2 = forAll sortedListsWithoutDuplicates $ isBalanced . astonishing

prop_astonishing_3 = forAll sortedListsWithoutDuplicates $ \ integers -> mysterious (astonishing integers) == integers

isBalanced :: BinaryTree -> Bool
isBalanced Leaf = True
isBalanced (Branch v l r) = abs (height l - height r) <= 1 && isBalanced l && isBalanced r
  where height Leaf = 0
        height (Branch v l r) = 1 + max (height l) (height r)
        
-- helper
quickCheck' prop
  = output <$>
    quickCheckWithResult stdArgs{chatty = False} prop 