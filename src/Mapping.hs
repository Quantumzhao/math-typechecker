module Mapping where

import Node
import ContextState
import Control.Monad.State.Lazy
import Set
import Tags
import Relation
-- data Mapping = Map Node Node Bijectivity

-- compose :: Mapping -> Mapping -> Maybe Mapping
-- compose (Map da ia ba) (Map db ib bb)
--   | ia == db = Just $ Map da ib (getBijectivity ba bb)
--   | otherwise = Nothing
--   where
--     getBijectivity any Bijective = any
--     getBijectivity Bijective any = any
--     getBijectivity Surjective Injective = undefined
--     getBijectivity Injective Surjective = undefined 
--     getBijectivity _ _ = None

-- setOfMappings :: Node
-- setOfMappings = Set (Type (Unary Definition universal universal [])) []


-- newMapping :: String -> Node -> Node -> [String] -> Context
-- newMapping name from to tags = do
--   let mapping = Unary Definition from to tags
--   addNewNode name mapping
--   return ()

-- short for cross, but yields an anonymous set
(<.>) :: Node -> Node -> Node
(<.>) n1 n2 = DirectProduct (n1, n2) Anonymous

crossFnDef :: Node
crossFnDef = Mapping {
  domain = allSets <.> allSets,
  range = allSets <.> allSets,
  tags = [surjectiveTag, injectiveTag],
  key = Unique "cross" "cross" 
}

intersectFnDef ::  Node
intersectFnDef = Mapping {
  domain = allSets <.> allSets,
  range = allSets,
  tags = [],
  key = Unique "intersect" "intersect"
}

unionFnDef :: Node
unionFnDef = Mapping (allSets <.> allSets) allSets [] (Unique "union" "union")

relComplFnDef :: Node
relComplFnDef = Mapping (allSets <.> allSets) allSets [] (Unique "minus" "minus")

relcompl :: Node -> Node -> String -> PContext Node
relcompl a b name
  | isSet a && isSet b = do
    id <- getNewId 
    let application = relComplFnDef
    let newNode = Class (tags a) (Unique name id)
    addNewStatementM (newNode `isSubsetOf` a)
    return newNode
  | otherwise = error "complement: not sets"
