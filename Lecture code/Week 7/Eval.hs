{-# LANGUAGE GADTs #-}

module Eval where

import Expr

eval :: Expr t -> t
eval (Const i)     = i
eval (Add e1 e2)   = eval e1 + eval e2
eval (Equal e1 e2) = eval e1 == eval e2
eval (If ec et ee) = 
    if eval ec then eval et else eval ee 
    
-- how to do it less generally without t 
-- looks a lot messier than the above cleaner soln 
-- eval (Add e1 e2) =
--    case (eval e1, eval e2) of 
--      (IntV i1, IntV i2) -> IntV $ i1 + i2
--      _                  -> error "Add: Int expected"
                       
--eval (Equal e1 e2) =
--    case (eval e1, eval e2) of
--      (IntV i1, IntV i2)   -> BoolV $ i1 == i2
--      (BoolV b1, BoolV b2) -> BoolV $ i1 == i2
--      _                    -> error "Equal: Same types expected"