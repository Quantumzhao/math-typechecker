{-# LANGUAGE ScopedTypeVariables #-}

module Set where

import Prelude as P
import ContextState
import Node as N
import Tags
import Control.Monad.Except

empty :: Node
empty = Class [setLit] (Exist "Empty" "Empty")

allSets :: Node
allSets = Class [] (Exist "AllSets" "AllSets")

setLit :: String
setLit = "Set"

isSet :: Node -> Bool
isSet (Class tags _) = setLit `elem` tags
isSet _ = P.False

anyClass :: Node
anyClass = Class [] (Exist "AnyClass" "AnyClass")

anything :: Node
anything = Class [] (Exist "Anything" "Anything")

{-| returns a relation actually -}
isSubsetOf :: Node -> Node -> PContext Node
isSubsetOf a b = do
  id <- getNewId
  let subsetRel = Relation a b orderedRel (Exist "isSubsetOf" id)
  return subsetRel

{-| returns a relation actually -}
isIn :: Node -> Node -> PContext Node
isIn x set = do
  id <- getNewId
  let isInRel = Relation x set [] (Exist "isIn" id)
  return isInRel

getElement :: Node -> String -> PContext Node
getElement set@(Class tags i) name = do
  newId <- getNewId
  if set == empty then throwError "Set.getElement: empty set"
  else do
    let e = Object (Exist name newId)
    addNewStatementM (e `isIn` set)
    return e
getElement _ _ = throwError "Set.getElement: not a set"

-- applyR'ed :: Node -> Node -> PContext Node
-- applyR'ed v binop@(Binary l r o ts name _) = do
--   subset <- isSubsetOf' v l  
--   newId <- getNewId 
--   if subset then
--     return $ Unary (App binop v) r o ts anonymous newId 
--   else error "apply binary"
-- applyR'ed v' un@(Unary u v o ts name _) = do
--   subset <- isSubsetOf' v' v
--   if subset then return o
--   else error "apply unary"
-- applyR'ed v rel@(Relation l r ts name _) = do
--   subset <- isSubsetOf' v l
--   newId <- getNewId 
--   if subset then 
--     return $ Class (App rel v) r ts anonymous newId
--   else error "apply relation"
-- applyR'ed v fixed@(Class u r ts name _) = do
--   subset <- isSubsetOf' r v 
--   newId <- getNewId 
--   if subset then
--     return $ Statement (App fixed v) ts anonymous newId
--   else error "apply fixed"
-- applyR'ed _ _ = error "apply no match" 

getNewSet :: [String] -> String -> PContext Node
getNewSet tags name = do
  id <- getNewId
  let coll = Class (setLit : tags) (Exist name id)
  return coll

-- every relation that relates this set from/to another structure, 
-- i.e. the relations that defines this set
-- which includes:
-- for all a in A, a R ?
-- A R ? 
getConstraints :: Node -> PContext [Node]
getConstraints c@Class {} = do
  let parent = undefined
  let child = undefined
  rels <- getRelations
  undefined
getConstraints _ = throwError "getConstraints: this only applies to sets"

isSameAs :: Node -> Node -> Bool
isSameAs a b = trackAlias a == trackAlias b

trackAlias :: Node -> Node
trackAlias (Alias ref _) = trackAlias ref
trackAlias n = n

addUniverse :: PContext Node
addUniverse = Class [] . Exist "Universe" <$> getNewId

addEmpty :: PContext Node
addEmpty = Class [setLit] . Exist "Empty" <$> getNewId

getUniverse :: PContext Node
getUniverse = getNodeByName "Universe"

getEmpty :: PContext Node
getEmpty = getNodeByName "Empty"
