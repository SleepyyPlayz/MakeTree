module TargetForestGeneration (
    TargetTree, getTargetName, getTargetDeps,
    getTargetForest,
) where

import Parsing.TargetParsing (Target, name, deps)

-- | Data structure for representing a SINGLE target dependency tree.
-- In the format of: TargetNode [name of target] [list of dependencies]
-- Note that a Makefile can have multiple of such trees!
data TargetTree = TargetNode String [TargetTree] deriving (Show)

-- | Helper function for TargetTree, returns the name of the target.
getTargetName :: TargetTree -> String
getTargetName (TargetNode t_name _) = t_name

-- | Helper function for TargetTree, returns the dependencies of the target.
getTargetDeps :: TargetTree -> [TargetTree]
getTargetDeps (TargetNode _ t_deps) = t_deps

-- | Returns all the target trees (as a "target forest") of a Makefile, given
-- all the targets in a List (along with dependencies of each).
-- IMPORTANT: A target can be the depency of multiple other targets. 
-- IMPORTANT: Assumes that there are NO CIRCULAR DEPENDENCIES, otherwise this
--            algorithm will not work.
getTargetForest :: [Target] -> [TargetTree]
getTargetForest = generateTargetForest []

-- | Recursive helper function for generateTargetForest, tail-recursively
-- constructs / generates the target forest. (Assuming no circular dependencies.)
-- Cases that can happen upon receiving a new target:
-- 1. Brand new target name (i.e. not anywhere in the forest), add it as a new tree.
--    Then look at its dependencies, for each dependency, if it is the root of any tree,
--    move that tree inside our new tree.
-- 2. Target name exists as one of the roots of the trees, in this case, first examine
--    all its dependencies as in Case 1 (moving trees out of existing forest and under
--    the relevant dependencies as needed), and then append the completed children
--    (dependency trees) as the children of the root of the existing tree.
--    And make sure to keep the order the same (i.e. append at the end).
-- 3. Target name exists as one or more of the non-root nodes (depencies) in any number 
--    of the trees. In this case, after expanding its dependencies into full trees
--    (similar to Case 1 and 2, and make sure the trees that are added in are removed
--    from the existing forest), append these full trees into the existing dependency
--    list of the non-root nodes in order.
-- Note that a target name cannot be both be in an existing root node and in one of
-- the existing dependency nodes, since this situation will be provably impossible due to
-- how the tree is generated.
generateTargetForest :: [TargetTree] -> [Target] -> [TargetTree]
generateTargetForest currentTrees [] = currentTrees
generateTargetForest currentTrees (target : targets) =
    let
        targetName = name target
        targetDeps = deps target  -- dependencies (also target names)

        -- TODO
    in
    undefined