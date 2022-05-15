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
    isSubsetOf' a b (ClaimOfRel from to _ _) = a `isSameAs` from && b `isSameAs` to
    isSubsetOf' a b _ = False

isInB :: Node -> Node -> PContext Bool
isInB e set
  | isSet e && set == allSets = return True
  | otherwise = do
    nodes <- getNodes
    isInRel <- get'isIn'relation
    return $ isJust $ findFirst (e `isIn'` set) nodes
    where
      isIn' a b (ClaimOfRel from to isInRel _) = a `isSameAs` from && b `isSameAs` to
      isIn' _ _ _ = False

get'isIn'relation :: PContext Node
get'isIn'relation = getNodeByName "isIn"

get'subsetOf'relation :: PContext Node
get'subsetOf'relation = getNodeByName "isSubsetOf"

existClaim :: Node -> PContext Bool
existClaim (ClaimOfRel from to rel _) = do
  let f (ClaimOfRel from' to' rel' _) = from == from' && to == to' && rel == rel'
      f _ = False
  isJust . findFirst f <$> getNodes
existClaim _ = throwError "existRelation: not a claim of relation"

-- use it as infix operator ?? `relatesTo` ?? `by` ??
relatesTo :: Node -> Node -> Node -> Node
relatesTo a b c = ClaimOfRel a b c ForAll
by = Prelude.id

-- gets all claims of relations which about this object
getAllRelatedClaims :: Node -> PContext Nodes
getAllRelatedClaims node = getNodes >>= filterM f where
  f r@(ClaimOfRel domain codomain relation id) = return $ node == domain || node == codomain
  f _ = return False

addIsInRel :: PContext ()
addIsInRel = do
  id <- getNewId
  u <- getUniverse
  let ret = Relation u allSets [] (Exist "isIn" id)
  addNewNode ret
  return ()

addIsSubsetRel :: PContext ()
addIsSubsetRel = do
  id <- getNewId
  let ret = Relation allSets allSets orderedRel (Exist "isSubsetOf" id)
  addNewNode ret
  return ()
