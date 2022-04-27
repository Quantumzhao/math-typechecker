{-# LANGUAGE ScopedTypeVariables #-}

module Set where

import Prelude as P
import GHC.Natural
import Control.Monad.State.Lazy
import qualified Data.List as List
import Data.List (intersperse, intercalate)
import ContextState
import Node as N
import Tags

empty :: Node
empty = Class [setLit] (Unique "Empty" "Empty")

allSets :: Node
allSets = Class [] (Unique "AllSets" "AllSets")

setLit :: String
setLit = "Set"

isSet :: Node -> Bool
isSet (Class tags _) = setLit `elem` tags
isSet _ = P.False

anyClass :: Node
anyClass = Class [] (Unique "AnyClass" "AnyClass")

{-| returns a relation actually -}
isSubsetOf :: Node -> Node -> PContext Node
isSubsetOf a b = do
  id <- getNewId
  let subsetRel = Relation a b orderedRel (Unique "subset" id)
  return subsetRel

{-| returns a relation actually -}
isIn :: Node -> Node -> PContext Node
isIn x set = do
  id <- getNewId 
  let isInRel = Relation x set [] (Unique "isIn" id) 
  return isInRel

getElement :: Node -> String -> PContext Node
getElement set@(Class tags i) name = do
  newId <- getNewId 
  if set == empty then error "Set.getElement: empty set"
  else do
    let e = Object set (Unique name newId)
    addNewStatementM (e `isIn` set)
    return e
getElement _ _ = error "Set.getElement: not a set"

-- powersetFnDef :: Node
-- powersetFnDef = 
--   let allSubsets = Class (App subsetFnDef universal) universal [] anonymous 0 in
--   Unary Definition universal (Set (Type allSubsets) []) []

-- powerset :: Node -> PContext Node
-- powerset set
--   | isSet set = do
--     nodes <- getNodes 
--     id <- getNewId 
--     application <-  set `applyR'ed` powersetFnDef
--     let newName = "P(" ++ name set ++ ")"
--     let newNode = Set (Type application) [] newName id
--     addNewStatement (set `isSubsetOf` newNode)
--     return newNode
--   | otherwise = error "powerset: not a set"

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
  let coll = Class (setLit : tags) (Unique name id)
  return coll
