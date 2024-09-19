module Main (main) where

-- import System.Directory (doesFileExist)
-- import System.Environment (getArgs)

-- import Lib

main :: IO ()
main = do
    putStrLn "Enter a file name : "
    inputFileName <- getLine

    fileContent <- readFile inputFileName

    putStrLn "Contents of the file : "
    putStrLn fileContent

    putStrLn "---------- EOF ----------"
