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
  addNewNode subsetRel
  return newNode
subset _ _ = throwError "Set.subset: not a set"

{-| returns a claim actually -}
isSubsetOf :: Node -> Node -> PContext Node
isSubsetOf a b = do
  rel <- get'subsetOf'relation
  let subsetRel = a `relatesTo` b `by` rel
  return subsetRel

isSubsetOfB :: Node -> Node -> PContext Bool
isSubsetOfB (DirectProduct (l, r) _) (DirectProduct (l', r') _) = do
  liftM2 (&&) (l `isSubsetOfB` l') (r `isSubsetOfB` r')
isSubsetOfB a b = do
  -- graph <- getNodes
  isSubsetRel <- get'subsetOf'relation
  existClaim (a `relatesTo` b `by` isSubsetRel)
  -- case findFirst (isSubsetOf' isSubsetRel a b) graph of
  --   Just _ -> return True
  --   Nothing -> return (a `isSameAs` b)
  -- where
  --   isSubsetOf' a b r (ClaimOfRel from to rel _) = a `isSameAs` from && b `isSameAs` to && r == rel
  --   isSubsetOf' _ _ _ _ = False

isInB :: Node -> Node -> PContext Bool
isInB (DirectProduct (l, r) _) (DirectProduct (l', r') _) = 
  liftM2 (&&) (l `isInB` l') (r `isInB` r')
isInB e set
  | isSet e && set == allSets = return True
  | otherwise = do
    isInRel <- get'isIn'relation
    existClaim (e `relatesTo` set `by` isInRel)
    -- nodes <- getNodes
    -- return $ isJust $ findFirst (isIn' isInRel e set) nodes
    -- where
    --   isIn' a b r (ClaimOfRel from to rel _) = a `isSameAs` from && b `isSameAs` to && r == rel
    --   isIn' _ _ _ _ = False

{-| returns a claim actually -}
isIn :: Node -> Node -> PContext Node
isIn x set = do
  rel <- get'isIn'relation
  let subsetRel = x `relatesTo` set `by` rel
  return subsetRel

get'isIn'relation :: PContext Node
get'isIn'relation = getNodeByName "isIn"

get'subsetOf'relation :: PContext Node
get'subsetOf'relation = getNodeByName "isSubsetOf"

existClaim :: Node -> PContext Bool
existClaim (ClaimOfRel from to rel _) =
  let f (ClaimOfRel from' to' rel' _) = from `isSameAs` from' && to `isSameAs` to' && rel == rel'
      f _ = False
  in isJust . findFirst f <$> getNodes
existClaim _ = throwError "existRelation: not a claim of relation"

-- use it as infix operator ?? `relatesTo` ?? `by` ??
relatesTo :: Node -> Node -> Node -> Node
relatesTo a b c = ClaimOfRel a b c (Exist "String" "String")
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
