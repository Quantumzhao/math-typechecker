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
  let rel = Relation relFrom relTo tags (Unique name id)
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
subsetFnDef = Relation allSets allSets orderedRel (Unique "subset" "subset")

subset :: Node -> String -> PContext Node
subset a@(Class tags _) name = do
  (nodes, idGen) <- get
  newId <- getNewId 
  let newNode = Class tags (Unique name newId)
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
    isSubsetOf' a b (Relation from to _ (Unique "subset" _)) = a == from && b == to
    isSubsetOf' a b _ = False

isInB :: Node -> Node -> PContext Bool
isInB e set = do
  nodes <- getNodes 
  case findFirst (e `isIn'` set) nodes of
    Just _ -> return True
    Nothing -> return False
  where 
    isIn' a b (Relation from to _ (Unique "isIn" _)) = a == from && b == to
    isIn' _ _ _ = False
