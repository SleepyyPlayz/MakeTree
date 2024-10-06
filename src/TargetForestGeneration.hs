module TargetForestGeneration (
    TargetTree, getTargetName, getTargetDeps,
    getTargetForest,
) where

import Data.List (find)

import Parsing.TargetParsing (Target, name, deps)


-- | Data structure for representing a SINGLE target dependency tree.
-- In the format of: TargetNode [name of target] [list of dependencies]
-- Note that a Makefile can have multiple of such trees!
data TargetTree = TargetNode String [TargetTree] deriving (Show)

-- | Helper function for TargetTree, returns the name of the target.
getTargetName :: TargetTree -> String
getTargetName (TargetNode targetName _) = targetName

-- | Helper function for TargetTree, returns the dependencies of the target.
getTargetDeps :: TargetTree -> [TargetTree]
getTargetDeps (TargetNode _ targetDeps) = targetDeps


-- | Returns all the target trees (as a "target forest") of a Makefile, given
-- all the targets in a List (along with dependencies of each).
-- IMPORTANT: A target can be the depency of multiple other targets. 
-- IMPORTANT: Assumes that there are NO CIRCULAR DEPENDENCIES, otherwise this
--            algorithm will not work.
getTargetForest :: [Target] -> [TargetTree]
getTargetForest = generateTargetForest []

-- | Recursive helper function for generateTargetForest, tail-recursively
-- constructs / generates the target forest. (Assuming no circular dependencies.)
--
-- Cases that can happen upon receiving a new target:
-- 1. Brand new target name (i.e. not anywhere in the forest), add it as a new tree.
--    But before we do that, look at its dependencies, for each dependency, if it is 
--    the root of any tree, move that tree inside our new tree.
-- 2. Target name exists as one of the roots of the trees, in this case, first examine
--    all its dependencies as in Case 1 (moving trees out of existing forest and under
--    the relevant dependencies as needed), and then append the completed child nodes
--    (subtrees for dependencies) to the children of the root of the existing tree.
--    And make sure to keep the order the same as in the Makefile.
-- 3. Target name exists as one or more of the non-root nodes (depencies) in any number 
--    of the trees. In this case, after expanding its dependencies into full trees
--    (similar to Case 1 and 2, and make sure the trees that are added in are removed
--    from the existing forest), append these full trees into the existing dependency
--    list of the non-root nodes in order.
--
-- Note that a target name cannot be both be in an existing root node and in one of
-- the existing dependency nodes, since this situation will be provably impossible due to
-- how the tree is generated from an empty tree.
generateTargetForest :: [TargetTree] -> [Target] -> [TargetTree]
generateTargetForest currentTrees [] = currentTrees
generateTargetForest currentTrees (target : targets) =
    let
        targetName = name target
        targetDeps = deps target

        nameInRoots = any ((== targetName) . getTargetName) currentTrees
        nameInForest = targetNameInForest targetName currentTrees

        -- Generating target tree based on the new target & its dependencies:
        newTargetSubtrees = [case find ((== depName) . getTargetName) currentTrees of
                                Just tree -> tree
                                Nothing   -> TargetNode depName []
                                | depName <- targetDeps]
        newTargetTree = TargetNode targetName newTargetSubtrees

        -- New version of currentTrees (the forest) with the trees that are inserted 
        -- into newTargetTree removed: (but before inserting anything)
        newForest = filter (\tree -> getTargetName tree `notElem` targetDeps) currentTrees 
    in
    if nameInRoots
    then 
        let
            -- Helper function to modify one of the trees in newForest:
            -- Note: we append newTargetSubtrees in the front since targets show up
            -- in reverse order.
            modificationFunc :: TargetTree -> TargetTree
            modificationFunc tree = if getTargetName tree == targetName 
                then TargetNode targetName (newTargetSubtrees ++ getTargetDeps tree)
                else tree
        in
        generateTargetForest (map modificationFunc newForest) targets  -- Case 2
    else 
        if nameInForest
        then 
            let 
                modificationFunc :: TargetTree -> TargetTree
                modificationFunc tree = tree  -- TODO: modify deps of all targetName subtrees
            in 
            generateTargetForest (map modificationFunc newForest) targets  -- Case 3
        else generateTargetForest (newTargetTree : newForest) targets  -- Case 1

-- | Helper function for generateTargetForest, returns whether a target is in a forest.
targetNameInForest :: String -> [TargetTree] -> Bool
targetNameInForest targetName = any (targetNameInTree targetName)

-- | Recursive helper function for targetNameInForest, returns whether a target is in
-- a single tree.
targetNameInTree :: String -> TargetTree -> Bool
targetNameInTree targetName tree = nameMatch || inSubtree
    where
        nameMatch = getTargetName tree == targetName
        inSubtree = any (targetNameInTree targetName) (getTargetDeps tree)
