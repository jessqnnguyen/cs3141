{-# LANGUAGE StandaloneDeriving #-} 

module Vec where
  
import Prelude hiding (reverse)

import Data.Proxy
import Data.Type.Equality 

import Numerals
import Proofs

data List a where
  Nil    :: List a
  -- cons case
  (::::) :: a -> List a -> List a
  
infixr 5 ::::
data Vec a (n :: Nat) where
  Null   :: Vec a Z
  (::::) :: a-> (Vec a n) -> (Vec a (S n))
  
deriving instance Show a => Show (Vec a n)

-- total function - cannot be called an empty vector
-- get the head of a vector whose length is n
vhead :: Vec a (S n) -> a
vhead (x :::: _) = x 

vtail :: Vec a (S n) -> Vec a n
vtail (_ ::: v) = v

vmap :: (a->b) --> Vec a n -> Vec b n
vmap f Null    = Null
vmap f (x ::: xs) = f x ::: vmap f xs

(+++) :: Vec a n -> Vec a m -> Vec a (n + m) 
Null +++ ys = ys
(x:::xs) +++ ys = x:::(xs +++ ys)

merge :: Ord a => Vec a n -> Vec a m -> Vec a (n + m)
merge Null vs = vs
merge (v ::: vs) (w ::: ws)
   | v <= w      = v ::: merge vs (w ::: ws)
   | otherwise = w ::: merge 

vec_id :: Vec a n1
       -> Vec a n2
       -> Vec a (n1 + S n2) :~: Vec a (S (n1 + n2))
vec_id v w = vec_lift 2 plus _succ_id v w 

-- lifts a type equality on SNats to Vecs
vec_lift :: (SNat n -> (k :~: l))
         -> Vec a n -> Vec a k :~: Vec a l

--prop_append xs ys 
--   = length xs + length ys == length (xs ++ ys) 
