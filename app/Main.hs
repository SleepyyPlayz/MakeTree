module Main (main) where

import System.Directory (doesFileExist)
-- import System.Environment (getArgs)

import Parsing.TargetParsing (getAllTargets)
import Conversion.TargetForestGeneration (getTargetForest)
import Conversion.Visualize (getDepTreeString)


-- | Entrypoint of application.
-- Note: This program assumes that any input Makefile is syntactically correct;
-- this can be checked by running "make --debug" in the directory of the Makefile.
main :: IO ()
main = do
    -- By default the program looks for Makefile or makefile in the current directory.
    -- Makefile takes priority over makefile.
    fileExists <- doesFileExist "Makefile"
    altFileExists <- doesFileExist "makefile"

    let fileName | fileExists = "Makefile" | altFileExists = "makefile" | otherwise = ""

    if fileName == ""
    then do
        putStrLn "ERROR: No Makefile or makefile exist in current directory."
    else do
        content <- readFile fileName
        putStrLn $ result content


-- | Helper function for main. Returns a single multi-line string for main to print,
-- given a file's contents as a string.
-- NOTE: Work in progress as the processing pipeline updates with new features.
result :: String -> String
result = getDepTreeString . getTargetForest . getAllTargets
