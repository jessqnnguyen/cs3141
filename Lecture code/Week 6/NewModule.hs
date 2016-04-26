module NewModule where

chars :: Char -> Char -> IO ()
chars x y = 
      if x < y 
      then putStrLn "yes"
      else putStrLn "no"

main = do
      x <- getChar
      y <- getChar
      chars x y 


