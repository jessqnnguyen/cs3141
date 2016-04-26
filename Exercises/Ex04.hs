module Ex04 where

import Data.Char
import System.Environment
import System.IO

capitalise :: FilePath -> FilePath -> IO ()
capitalise path1 path2 = do
      contents <- readFile path1
      writeFile path2 (map toUpper contents)

-- retrieve two filenames which are given as a command line argument, read the file specified in the first command line argument. That files must contain a list of integers, one per line, and print the sum of these numbers into the second file given as a command line argument.
sumFile :: IO ()
sumFile = do
  (file1:args) <- getArgs
  contents <- readFile file1
  let allLines = lines contents
      numbers = map read allLines
      sumNumbers = show $ sum numbers
      (file2:xs) = args
  writeFile file2 sumNumbers


