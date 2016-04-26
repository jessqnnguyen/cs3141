{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Numerals
where
 
data Nat where
   Z :: Nat
   S :: Nat -> Nat
   
-- A singleton natural number 
-- Successor of Nat data constructor 
data SNat (n :: Nat) where
  Zero :: SNat Z
  Succ :: SNat m -> SNat (S m)
 
deriving instance (Show Nat)
deriving instance (Show (SNat n))

zero  = Zero
one   = Succ zero
two   = Succ one
three = Succ two

-- Addition just using types! 
addTwoNat :: Nat -> Nat
addTwoNat m = S (S m)

addTwo :: SNat n -> SNat (S (S n))
addTwo m = Succ (Succ m)

add :: SNat n -> SNat m -> SNat (n + m)
add Zero y  = y
add (Succ x) y = Succ (add x y)

type family (+) (n :: Nat) (m:: Nat) :: Nat
type instance Z     + m = m 
type instance (S n) + m =   S (n + m)

sub :: SNat (n + m) -> SNat n -> SNat m
sub x        Zero     = x
sub (Succ x) (Succ y) = sub x y