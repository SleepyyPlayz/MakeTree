module Main (main) where

import System.Directory (doesFileExist)
-- import System.Environment (getArgs)

-- import Lib

-- | Entrypoint of application.
-- Note: This program assumes that any input Makefile is syntactically correct;
-- this can be checked by running "make --debug" in the directory of the Makefile.
main :: IO ()
main = do
    putStrLn "Enter the filepath of a VALID Makefile (must be valid):"
    filePath <- getLine

    -- TODO: check if it's a valid Makefile
    fileExists <- doesFileExist filePath

    if not fileExists
    then putStrLn "Invalid file path. Please try again."
    else do
        fileContent <- readFile filePath
        putStrLn "Contents of the file : "
        putStrLn fileContent
