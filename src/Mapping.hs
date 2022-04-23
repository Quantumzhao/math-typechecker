module Mapping where

import Node
import ContextState
import Control.Monad.State.Lazy
import Set
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

{-| anonymous cross product -}
crossA :: Node -> Node -> Node
crossA a b
  | isSet a && isSet b =
    let (Mapping _ resultantType _ _) = crossFnDef a b in
    Collection (FormOf resultantType) [setLit] Anonymous
  | otherwise = error "cross': not sets"

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
    id <- getNewId 
    let (Mapping _ resultantType _ _) = crossFnDef a b
    let newNode = Collection (FormOf resultantType) [setLit] (Unique name id)
    return newNode
  | otherwise = error "cross: not sets"

intersectFnDef :: Node -> Node -> Node
intersectFnDef type1 type2
  | isSet type1 && isSet type2 = Mapping {
      domain = type1 `crossA` type2,
      range = anySet,
      tags = [],
      key = Unique "intersect" "intersect"
  }
  | otherwise  = error "intersectFnDef: not set"

intersect :: Node -> Node -> String -> PContext Node
intersect a b name
  | isSet a && isSet b = do
    id <- getNewId
    let application = intersectFnDef a b
    let newNode = Collection (FormOf application) (tags a ++ tags b) (Unique name id)
    addNewStatementM (newNode `isSubsetOf` a)
    addNewStatementM (newNode `isSubsetOf` b)
    return newNode
  | otherwise = error "intersect: not sets"

-- substitute :: Node -> Node -> PContext Node
-- substitute x binop@(Mapping dom ran tags i) = do
--   isSubset <- x `isSubsetOf'` dom
--   isElem <- x `isIn'` dom
--   newId <- getNewId 
--   if isSubset || isElem then return $ Mapping x ran tags name newId
--   else error "Set.substitute: not a subset of domain"
-- substitute x _ = error "Set.substitute: not a mapping"

unionFnDef :: Node -> Node -> Node
unionFnDef a b
  | isSet a && isSet b = Mapping (a `crossA` b) anySet [] (Unique "union" "union")
  | otherwise = error "unionFnDef: not sets"

union :: Node -> Node -> String -> PContext Node
union a b name
  | isSet a && isSet b = do
    id <- getNewId 
    let application = unionFnDef a b
    let newNode = Collection (FormOf application) [] (Unique name id)
    addNewStatementM (a `isSubsetOf` newNode)
    addNewStatementM (b `isSubsetOf` newNode)
    return newNode
  | otherwise = error "union: not sets"

complementFnDef :: Node -> Node -> Node
complementFnDef type1 type2 = Mapping (type1 `crossA` type2) anySet [] (Unique "minus" "minus")

complement :: Node -> Node -> String -> PContext Node
complement a b name
  | isSet a && isSet b = do
    id <- getNewId 
    let application = complementFnDef a b
    let newNode = Collection (FormOf application) (tags a) (Unique name id)
    addNewStatementM (newNode `isSubsetOf` a)
    return newNode
  | otherwise = error "complement: not sets"
