{-# LANGUAGE ScopedTypeVariables #-}
module Relation where
import Node
import ContextState
import Set
import Control.Monad.State.Lazy
import Tags

genRelation :: Node -> Node -> [String] -> String -> PContext Node
genRelation relFrom relTo tags name = do
  id <- getNewId 
  let rel = Relation relFrom relTo tags (Exist name id)
  return rel

-- applyR (Relation dom cod tags name _) b = do
--   nodes <- getNodes
--   if elems equivalenceRel tags then do
--     newId <- getNewId 
--     newId' <- getNewId
--     let newNode :: Node = Set (FormOf $ Relation dom b tags name newId) [eqClass] ("eq " ++ nameOf dom) newId'
--     addNewStatement (newNode `isSubsetOf` dom)
--     return eqClass
--   else undefined
-- applyR _ _ = error "Relation.applyR: not a relation"

-- -- a ~ ?
-- applyL (Relation dom cod tags name _) a = do
-- applyL _ a = error "Relation.applyL: not a relation"

subsetFnDef :: Node
subsetFnDef = Relation allSets allSets orderedRel (Exist "subset" "subset")

subset :: Node -> String -> PContext Node
subset a@(Class tags _) name = do
  (nodes, idGen) <- get
  newId <- getNewId 
  let newNode = Class tags (Exist name newId)
  subsetRel <- newNode `isSubsetOf` a
  addNewStatement subsetRel
  return newNode
subset _ _ = error "Set.subset: not a set"

isSubsetOfB :: Node -> Node -> PContext Bool
isSubsetOfB a b = do
  graph <- getNodes
  case findFirst (a `isSubsetOf'` b) graph of
    Just _ -> return True
    Nothing -> return False
  where
    isSubsetOf' a b (Relation from to _ (Exist "subset" _)) = a == from && b == to
    isSubsetOf' a b _ = False

isInB :: Node -> Node -> PContext Bool
isInB e set
  | isSet e && set == allSets = return True
  | otherwise = do
  nodes <- getNodes 
  case findFirst (e `isIn'` set) nodes of
    Just _ -> return True
    Nothing -> return False
  where 
    isIn' a b (Relation from to _ (Exist "isIn" _)) = a == from && b == to
    isIn' _ _ _ = False

get'isIn'relation :: Graph -> Node
get'isIn'relation nodes = 
  case findByName "isIn" nodes of
    Just n -> n
    Nothing -> error "get'isIn'relation: isIn hasn't been defined yet"

get'subsetOf'relation :: Graph -> Node
get'subsetOf'relation nodes =
  case findByName "subset" nodes of
    Just n -> n
    Nothing -> error "get'subsetOf'relation: subsetOf hasn't been defined yet"

existRelation :: Node -> Graph -> Bool
existRelation (Relation a b tags (Exist name id)) nodes = 
  let f (Relation a' b' tags' (Exist name' id')) = a == a' &&
                                                    b == b' &&
                                                    name == name'
      f _ = False
  in case findFirst f nodes of
    Just _ -> True
    Nothing -> False
existRelation _ _ = error "existRelation: not a relation"
