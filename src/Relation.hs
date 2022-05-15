{-# LANGUAGE ScopedTypeVariables #-}
module Relation where
import Node
import ContextState
import Set
import Control.Monad.State.Lazy
import Tags
import Control.Monad.Except
import Data.Maybe

genRelation :: Node -> Node -> [String] -> String -> PContext Node
genRelation relFrom relTo tags name = do
  id <- getNewId
  let rel = Relation relFrom relTo tags (Exist name id)
  return rel

subsetFnDef :: Node
subsetFnDef = Relation allSets allSets orderedRel (Exist "subset" "subset")

subset :: Node -> String -> PContext Node
subset a@(Class tags _) name = do
  newId <- getNewId
  let newNode = Class tags (Exist name newId)
  subsetRel <- newNode `isSubsetOf` a
  addNewStatement subsetRel
  return newNode
subset _ _ = throwError "Set.subset: not a set"

isSubsetOfB :: Node -> Node -> PContext Bool
isSubsetOfB a b = do
  graph <- getNodes
  case findFirst (a `isSubsetOf'` b) graph of
    Just _ -> return True
    Nothing -> return (a `isSameAs` b)
  where
    isSubsetOf' a b (Relation from to _ (Exist "subset" _)) = a `isSameAs` from && b `isSameAs` to
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
    isIn' a b (Relation from to _ (Exist "isIn" _)) = a `isSameAs` from && b `isSameAs` to
    isIn' _ _ _ = False

get'isIn'relation :: Nodes -> Node
get'isIn'relation nodes =
  case findByName "isIn" nodes of
    Just n -> n
    Nothing -> error "get'isIn'relation: isIn hasn't been defined yet"

get'subsetOf'relation :: Nodes -> Node
get'subsetOf'relation nodes =
  case findByName "subset" nodes of
    Just n -> n
    Nothing -> error "get'subsetOf'relation: subsetOf hasn't been defined yet"

existClaim :: Node -> PContext Bool
existClaim c@ClaimOfRel {} = do
  let f c'@ClaimOfRel {} = c == c'
      f _ = False
  isJust . findFirst f <$> getNodes
existClaim _ = throwError "existRelation: not a claim of relation"

-- use it as infix operator ?? `relatesTo` ?? `by` ??
relatesTo = ClaimOfRel
by = Prelude.id

-- gets all claims of relations which about this object
getAllRelatedClaims :: Node -> PContext Nodes
getAllRelatedClaims node = getNodes >>= filterM f where
  f r@(ClaimOfRel domain codomain relation) = return $ node == domain || node == codomain
  f _ = return False
