module Relation where
import Node ( Identifier(..), Node(..) )
import ContextState
  ( PContext,
    Nodes,
    addNewNode,
    findFirst,
    getNodes,
    getNewId,
    getNodeByName )
import Set ( allSets, getUniverse, isSameAs, isSet )
import Control.Monad.State.Lazy ( liftM2, filterM )
import Tags ( orderedRel )
import Control.Monad.Except ( MonadError(throwError) )
import Data.Maybe ( isJust )

-- generate a relation
genRelation :: Node -> Node -> [String] -> String -> PContext Node
genRelation relFrom relTo tags name = do
  id <- getNewId
  let rel = Relation relFrom relTo tags (Exist name id)
  return rel

{-| returns a claim actually -}
-- use it as an infix operator
isSubsetOf :: Node -> Node -> PContext Node
isSubsetOf a b = do
  rel <- get'subsetOf'relation
  let subsetRel = a `relatesTo` b `by` rel
  return subsetRel

-- check if one set is the subset of another
isSubsetOfB :: Node -> Node -> PContext Bool
-- the case where the two arguments are cartesian products
isSubsetOfB (DirectProduct (l, r) _) (DirectProduct (l', r') _) = do
  liftM2 (&&) (l `isSubsetOfB` l') (r `isSubsetOfB` r')
-- noraml case
isSubsetOfB a b = do
  -- graph <- getNodes
  isSubsetRel <- get'subsetOf'relation
  existClaim (a `relatesTo` b `by` isSubsetRel)

-- similar to `isSubsetOfB`
isInB :: Node -> Node -> PContext Bool
isInB (DirectProduct (l, r) _) (DirectProduct (l', r') _) = 
  liftM2 (&&) (l `isInB` l') (r `isInB` r')
isInB e set
  | isSet e && set == allSets = return True
  | otherwise = do
    isInRel <- get'isIn'relation
    existClaim (e `relatesTo` set `by` isInRel)

{-| returns a claim actually -}
-- similar to `isSubsetOf`
isIn :: Node -> Node -> PContext Node
isIn x set = do
  rel <- get'isIn'relation
  let subsetRel = x `relatesTo` set `by` rel
  return subsetRel

get'isIn'relation :: PContext Node
get'isIn'relation = getNodeByName "isIn"

get'subsetOf'relation :: PContext Node
get'subsetOf'relation = getNodeByName "isSubsetOf"

-- check if the specified claim exist
existClaim :: Node -> PContext Bool
existClaim (ClaimOfRel from to rel _) =
  -- use `isSameAs` because nodes can be aliases
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

-- these two relations are hardcoded because there is no good way of describing them in my DSL

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
