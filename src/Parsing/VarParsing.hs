module Parsing.VarParsing (
    VarMapping,
    getVarMappings,
) where

-- import Data.List (isPrefixOf)

-- | Datatype that maps a Makefile variable's name to its value
data VarMapping = Var String String

-- | Returns all variable mappings that can be extrapolated from a Makefile, 
-- | given a syntactically correct Makefile that was read into a single String
-- | via readFile from System.IO.
-- | Returns [] when there are no variable mappings in the Makefile.
getVarMappings :: String -> [VarMapping]
getVarMappings _ = undefined  -- TODO: implement
