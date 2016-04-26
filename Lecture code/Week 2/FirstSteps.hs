import Prelude hiding (length, map)

inc :: Int -> Int
inc x = x + 1

exclaim str = str ++ "!"

-- This function's main type is IO
main :: IO ()
main = putStrLn (exclaim "Haskell")

-- This function is called average and computes the average of 2 numbers
average :: Double -> Double -> Double
average x y = (x + y) / 2

-- Create an abbreviation for calling average() with first parameter 10
let av10 = average 10
-- average(10, 20)
av10 20

-- Higher order functions
applyTwice :: (Double -> Double) -> Double -> Double
applyTwice fn x = fn (fn x)

-- Lists
-- Returns length of the list 
length :: [a] -> Int 
length []	= 0
length (x:xs) = length xs + 1

-- Call length
length [1, 2, 3, 4] -- = 4
 
-- Apply a to every element in b
map :: (a -> b) -> [a] -> [b]
map f []	= [] -- Empty list case 
map f(x:xs) = f x 	map f xs 

-- Call map
map (\x -> x + 1) [1, 2, 3, 4, 5] -- [2, 3, 4, 5]
-- Can also be alternatively called like this:
map (+1) [1, 2, 3, 4, 5]

-- User-defined types
type Point = (Float, Float)
type Path = [Point]

-- Data types can be like structs in C ( we call these data types product types)
data Point2 = MkPoint Float Float 

-- Data types can be like unions in C (we call those data types sum types)
data Host = MkNumericIP Int Int Int Int
 		  | MkSymbolicIP String 

-- Product-Sum types
-- These are Haskell's data types
-- They can be recursive
-- They're a lot like generics in Java and data types can be parameterised
data Maybe a = Nothing | Just a 

-- Identifiers in Haskell
-- Alphanumeric with underscores(_)  and prime sympols (')
-- Case matters
-- Functions and variables are always in lower case e.g. map, pi, (+), (++)
-- Data constructers are always capitalised (first letter) e.g. True, Nothing, (:)
-- Type variables are always in camelcase e.g. a, b, c, eltType
-- Type constructors are always capitalised (firstLetter) e.g. Int, Bool, IO
