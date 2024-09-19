module Parsing.PhonyParsing (
    getPhonyTargets,
) where

import Data.List (isPrefixOf)

-- | Returns the names of all the .PHONY targets in the Makefile, given a syntactically
-- correct Makefile that was read into a single String via readFile from System.IO.
-- Returns [] when there are no .PHONY targets.
getPhonyTargets :: String -> [String]
getPhonyTargets fileContent = 
    let 
        -- First we obtain all the lines in the file, keep in mind some are empty.
        allFileLines = lines fileContent

        -- Note: There can be multiple .PHONY lines in a Makefile.
        linesThatMatter = filter (\line -> ".PHONY: " `isPrefixOf` line) allFileLines

        -- Now we break down each line into its tokens, and remove the ".PHONY:" part.
        -- Note that the 'words' function removes ALL space deliminers between tokens.
        tokensPerLine = map (drop 1 . words) linesThatMatter

        -- and bring the all tokens across different lines into a single List.
        allTokens = concat tokensPerLine
    in
    allTokens
