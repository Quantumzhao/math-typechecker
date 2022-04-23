module Mapping where

import Node
import ContextState
import Control.Monad.State.Lazy
import Set
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

-- no bijective tag because it should be replaced by injective and surjective in the AST
surjectiveTag :: String
surjectiveTag = "surjective"
injectiveTag :: String
injectiveTag = "injective"

-- newMapping :: String -> Node -> Node -> [String] -> Context
-- newMapping name from to tags = do
--   let mapping = Unary Definition from to tags
--   addNewNode name mapping
--   return ()

crossFnDef :: Node -> Node -> Node
crossFnDef type1 type2
  | isSet type1 && isSet type2 = Mapping {
      domain = DirectProduct (type1, type2) Anonymous,
      range = DirectProduct (type1, type2) Anonymous,
      tags = [surjectiveTag, injectiveTag],
      key = Unique "cross" "cross" 
  }
  | otherwise = error "crossFnDef: not set"

cross :: Node -> Node -> String -> PContext Node
cross a b
  | isSet a && isSet b = \name -> do
    nodes <- getNodes 
    id <- getNewId 
    let (Mapping _ resultantType _ _) = crossFnDef a b
    let newNode = Collection (FormOf resultantType) [setLit] (Unique name id)
    return newNode
  | otherwise = error "cross: not sets"

intersectFnDef :: Node -> Node -> Node
intersectFnDef type1 type2
  | isSet type1 && isSet type2 = Mapping {
      domain = DirectProduct (type1, type2) Anonymous,
      range = undefined,
      tags = [],
      key = Unique "intersect" "intersect"
  }
  | otherwise  = error "intersectFnDef: not set"

-- intersect :: Node -> Node -> PContext Node
-- intersect a b
--   | isSet a && isSet b = do
--     id <- getNewId 
--     ab <- cross a b
--     application <- substitute intersectFnDef ab
--     let newName = nameOf a ++ " intersect " ++ nameOf b
--     let newNode = Set (FormOf application) (tags a ++ tags b) newName id
--     addNewStatement (newNode `isSubsetOf` a)
--     addNewStatement (newNode `isSubsetOf` b)
--     return newNode
--   | otherwise = error "intersect: not sets"

