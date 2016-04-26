module Words where
  
import Test.QuickCheck
import Data.Char

prop_Words s = 
  unwords (words s) == s
  
prop_Words' s = 
  -- Check for test string s that it doesn't
  -- have a space at either end
  -- . operator is used for function composition
  -- combines two functions not and isNotASCIISpace
  -- Another alternative is 
  -- \c -> not (isNotASCIISpace c)) s 
  -- is the same as
  -- (not . isNotASCIISpace) s
  validString s && all (not . isNotASCIISpace) s ==> 
  -- all gets the predicate
  unwords (words s) == s
  where
    -- An empty string is always valid
    validString "" = True
    validString s  = not (isSpace (last s)) &&
                     not (isSpace (head s))
    isNotASCIISpace c = isSpace c && c /= ' ' 
    -- /= is != 
  
-- helper
quickCheck' prop 
  = output <$>
    quickCheckWithResult stdArgs{chatty = False}      
    prop
    