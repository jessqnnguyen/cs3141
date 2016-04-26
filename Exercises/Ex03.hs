module Ex03 (
  BinaryTree(..), isBST, insertBST, searchTrees,
  prop_insert_on_empty_tree, prop_insert_preserves_bst, prop_insert_adds_element, prop_insert_does_not_change_other_elements,
  prop_insert_duplicate_check,
  prop_delete_detect_error, prop_delete_preserves_bst, prop_delete_removes_element, prop_delete_does_not_change_other_elements,
  height, size
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

insertBST :: Integer -> BinaryTree -> BinaryTree
insertBST i Leaf = Branch i Leaf Leaf
insertBST i (Branch v l r) 
  | i < v = Branch v (insertBST i l) r
  | otherwise = Branch v l (insertBST i r)   

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where 
   searchTrees' 0 = return Leaf
   searchTrees' n = do 
      v <- (arbitrary :: Gen Integer)
      fmap (insertBST v) (searchTrees' $ n - 1)
      
contains :: Integer -> BinaryTree -> Bool
contains i Leaf = False
contains i (Branch v l r)
    | i == v = True
    | i < v = contains i l
    | otherwise = contains i r
    
containsCopiesOf :: Integer -> BinaryTree -> Integer
containsCopiesOf i Leaf = 0
containsCopiesOf i (Branch v l r)
    | i == v = 1 + containsCopiesOf i l + containsCopiesOf i r
    | i < v = containsCopiesOf i l 
    | i > v = containsCopiesOf i r

--------------

prop_insert_on_empty_tree insertFunction integer = (insertFunction integer Leaf) == Branch integer Leaf Leaf

prop_insert_preserves_bst insertFunction integer = 
  forAll searchTrees $ \tree -> isBST (insertFunction integer tree)

prop_insert_adds_element insertFunction integer = 
  forAll searchTrees $ \tree -> contains integer (insertFunction integer tree)

prop_insert_does_not_change_other_elements insertFunction integer newInteger = 
  forAll searchTrees $ \tree -> contains integer (insertFunction newInteger (insertFunction integer tree))

-- to check for duplicates
-- insert integer two times so there should be at least 2 copies of the integer inside the BST
prop_insert_duplicate_check insertFunction integer =
  forAll searchTrees $ \tree -> containsCopiesOf integer (insertFunction integer (insertFunction integer tree)) >= 2

------------

prop_delete_detect_error deleteFunction i1 i2 = 
  i1 /= i2 ==>
    deleteFunction i1 (Branch i2 Leaf Leaf) == Branch i2 Leaf Leaf 

prop_delete_preserves_bst deleteFunction integer = 
  forAll searchTrees $ \tree -> isBST (deleteFunction integer tree)

prop_delete_removes_element deleteFunction integer = 
  forAll searchTrees $ \tree -> (containsCopiesOf integer tree) - (containsCopiesOf integer (deleteFunction integer tree)) == 1

prop_delete_does_not_change_other_elements deleteFunction integer newInteger =
  forAll searchTrees $ \tree -> (contains integer (deleteFunction newInteger tree)) == True

----------------

incorrectDelete :: Integer -> BinaryTree -> BinaryTree
incorrectDelete i (Leaf) = Leaf
incorrectDelete i (Branch v l r) = Leaf

incorrectDelete2 :: Integer -> BinaryTree -> BinaryTree
incorrectDelete2 i (Leaf) = Leaf
incorrectDelete2 i (Branch v l r) 
      | i == v = incorrectDelete2 i l 
      | i < v = incorrectDelete2 i r
      | otherwise = incorrectDelete2 i l

insert1 :: Integer -> BinaryTree -> BinaryTree
insert1 i Leaf = Leaf
insert1 i (Branch v l r) 
       | i < v = Branch v (insert1 i l) r
       | otherwise =  Branch v l (insert1 i r)
 
insert2 :: Integer -> BinaryTree -> BinaryTree
insert2 i Leaf = Branch i Leaf Leaf
insert2 i (Branch v l r) 
       | i < v = Branch v (insert2 i l) r
       | i > v = Branch v l (insert2 i r)
   
insert3 :: Integer -> BinaryTree -> BinaryTree
insert3 i Leaf = Branch i Leaf Leaf
insert3 i (Branch v l r) 
       | i < v = Branch v (insert3 i l) r
       | i == v = Branch v l r
       | i > v = Branch v l (insert3 i r)
   
insert4 :: Integer -> BinaryTree -> BinaryTree
insert4 i Leaf = Branch i Leaf Leaf
insert4 i (Branch v l r) 
       | i < v = Branch v l (insert4 i r)
       | otherwise = Branch v (insert4 i l) r

height :: BinaryTree -> Integer
height Leaf = 0
height (Branch v l r) = 1 + max (height l) (height r) 

size :: BinaryTree -> Integer
size Leaf = 0
size (Branch v l r) = 1 + (size l) + (size r)

main :: IO ()
main = quickCheck $ prop_insert_on_empty_tree insertBST
--      .&. prop_insert_preserves_bst insert1
--      .&. prop_insert_adds_element insert3
--      .&. prop_insert_does_not_change_other_elements insert1
--      .&. prop_insert_duplicate_check insert1
--      .&. prop_delete_preserves_bst incorrectDelete
--      .&. prop_delete_detect_error incorrectDelete 1 2
--      .&. prop_delete_removes_element incorrectDelete
      .&. prop_delete_does_not_change_other_elements incorrectDelete2
      