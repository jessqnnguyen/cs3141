{-# LANGUAGE GADTs #-}

module Expr where

-- Simple expressions
data Expr t where
  Const ::         Int                           -> Expr Int
  Add   ::         Expr Int -> Expr Int          -> Expr Int
  Equal :: Eq t => Expr t -> Expr t              -> Expr Bool
  If    ::         Expr Bool -> Expr t -> Expr t -> Expr t

-- Eq t enforces that t must be of type of Eq - this is an example of a precondition
-- Expr Int at the end enforces that the end value of the express must be of type int
-- This is an example of a post condition


-- Evaluation results  
data Value = IntV Int
            | BoolV Bool
            deriving Show

-- Sample expressions
add = Add (Const 40) (Const 2)
cond = If (Equal (Const 42) add) (Const 1) (Const 0)
err = Equal (Equal (Const 1) (Const 2)) (Const 3)