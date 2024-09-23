module Parsing.TargetParsing (
    Target, name, deps,  -- data type and access functions
    getAllTargets,
) where

import Data.Char (isSpace)
import Data.List (isInfixOf)

-- | Datatype that describes a target, with its dependencies (in order).
data Target = Target { name :: String, deps :: [String] } deriving (Show)

-- | Returns all the targets and their dependencies in the Makefile, given 
-- a syntactically correct Makefile that was read into a single String via
-- readFile from System.IO.
-- Returns [] when there are no targets.
-- TODO:
--     Does not currently support usage of $(variables) and % in the names or
--     dependencies of the targets. Those features are to be implemented later.
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
    extractDependencies [] targetLines

-- | Helper function that returns whether a makefile's non-tabbed line is a line
-- that defines a target (with or without its dependentices).
-- Note that sometimes, there are lines where we create temporary variable bindings
-- for a target specifically, that can look like:
--     target: var = value
--     target: var := value
-- These do not define its dependencies. (But the target still exists.)
isTargetLine :: String -> Bool
isTargetLine line = (length firstToken >= 2) && (last firstToken == ':')
    where firstToken = head (words line)

-- | Recursive function that extracts targets & their dependencies in a LINE-BY-LINE
-- fasion, given all targets already extrapolated and the List of all lines 
-- that contains a target name. (This allows for tail recursion.)
-- Also keep in mind the case where there are temporary variable bindings for a target.
-- In this case the target might still need to be added, but no dependencies shoule be
-- added.
extractDependencies :: [Target] -> [String] -> [Target]
extractDependencies existingTargets [] = existingTargets
extractDependencies existingTargets (currentLine : remainingLines) =
    let
        -- Precondition: There is guaranteed to be a target name, before a ':'.
        firstToken = head (words currentLine)  -- contains ':' at the end
        targetName = init firstToken           -- removes the ':' character

        -- Tokens after the name of the target (in order), can be empty.
        tokens = tail (words currentLine)

        -- Function to check if a line is used to define bindings & not dependencies.
        isBindingLine :: String -> Bool
        isBindingLine line = (" = " `isInfixOf` line) || (" := " `isInfixOf` line)
    in
    if elem targetName $ map name existingTargets  -- see if target is already recorded
    then
        if isBindingLine currentLine
        then extractDependencies existingTargets remainingLines  -- skip
        else
            let
                -- The following is a way to modify one element of a List with "map":
                modificationFunc :: Target -> Target
                modificationFunc t = if name t == targetName
                    then Target { name = targetName, deps = deps t ++ tokens }
                    else t

                modifiedTargets = map modificationFunc existingTargets
            in
            extractDependencies modifiedTargets remainingLines
    else
        if isBindingLine currentLine
        then
            let newTarget = Target { name = targetName, deps = [] }
            in extractDependencies (newTarget : existingTargets) remainingLines
        else
            let newTarget = Target { name = targetName, deps = tokens }
            in extractDependencies (newTarget : existingTargets) remainingLines
