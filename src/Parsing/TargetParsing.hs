module Parsing.TargetParsing (
    getAllTargets,
) where

import Data.Char (isSpace)

-- | Datatype that describes a target, with its dependencies
data Target = Target { name :: String, dependencies :: [String] }

-- | Returns all the targets and their dependencies in the Makefile, given 
-- | a syntactically correct Makefile that was read into a single String via
-- | readFile from System.IO.
-- | Returns [] when there are no targets.
-- | IMPORTANT NOTES:
-- |     Does not currently support usage of $(variables) and % in the names or
-- |     dependencies of the targets. Those features are to be implemented later.
getAllTargets :: String -> [Target]
getAllTargets fileContent = 
    let
        -- First we obtain all the non-empty lines in the file, then the lines in
        -- the file that do not only contain whitespace.
        nonEmptyLines = filter (not . null) (lines fileContent)
        nonBlankLines = filter (not . all isSpace) nonEmptyLines

        -- Next we obtain all lines that do not start with a tab "\t" character,
        -- i.e. lines that are not commands to be run for building a target.
        nonTabbedLines = filter (\line -> head line /= '\t') nonBlankLines

        -- Then we find all lines where the first token's last character is ':',
        -- followed before by a name (i.e. there are characters before the ':').
        targetLines = filter isTargetLine nonTabbedLines
    in 
    targetLines

-- | Helper function that returns whether a makefile's non-tabbed line is a line 
-- | that defines a target with its dependentices. 
-- | Note that sometimes, there are lines where we create 
isTargetLine :: String -> Bool
isTargetLine _ = undefined
