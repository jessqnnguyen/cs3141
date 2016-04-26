module FirstSteps where 
  
square :: Int -> Int
square x = x * x

inc :: Num a => a -> a
inc x = x + 1

average :: Fractional a => a -> a -> a
average x y   = (x + y) / 2.0

showResult :: Show a => a -> String
showResult result = "The result is " ++ show result