{-# LANGUAGE StandaloneDeriving #-}

module Proofs where
  
import Data.Proxy
import Data.Type.Equality

import Numerals

data t :~: s where
  Refl :: t~: t

-- Add right
-- ----------
addR :: SNat m -> Snat n -> SNat (m + n)
addR = undefined

plus_succ_id :: SNat n1 -> SNat n2 -> (n1 + S n2) :~: (S (n1 + n2))
plus_succ_id = undefined

plus_id_r :: SNat n -> (n + Z) :~: n
plus_id_r = undefined 




