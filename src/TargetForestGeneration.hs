module TargetForestGeneration (
    TargetTree, getTargetName, getTargetDeps,
    getTargetForest,
) where

import Parsing.TargetParsing (Target)

-- | Data structure for representing a SINGLE target dependency tree.
-- | In the format of: TargetNode [name of target] [list of dependencies]
-- Note that a Makefile can have multiple of such trees!
data TargetTree = TargetNode String [TargetTree] deriving (Show)

-- | Helper function for TargetTree, returns the name of the target.
getTargetName :: TargetTree -> String
getTargetName (TargetNode name _) = name

-- | Helper function for TargetTree, returns the dependencies of the target.
getTargetDeps :: TargetTree -> [TargetTree]
getTargetDeps (TargetNode _ deps) = deps

-- | Returns all the target trees (as a "target forest") of a Makefile, given
-- | all the targets in a List (along with dependencies of each).
-- IMPORTANT: A target can be the depency of multiple other targets. 
-- IMPORTANT: Assumes that there are NO CIRCULAR DEPENDENCIES, otherwise this
--            naive algorithm will not work.
getTargetForest :: [Target] -> [TargetTree]
getTargetForest = generateTargetForest []

-- | Recursive helper function for generateTargetForest, tail-recursively
-- | constructs / generates the target forest.
generateTargetForest :: [TargetTree] -> [Target] -> [TargetTree]
generateTargetForest currentTrees [] = currentTrees
generateTargetForest currentTrees (target : targets) = undefined  -- TODO