{-# LANGUAGE ScopedTypeVariables #-}

module Set where

import Prelude as P
import GHC.Natural
import Control.Monad.State.Lazy
import qualified Data.List as List
import Data.List (intersperse, intercalate)
import ContextState
import Node as N
import Data.Map
import Tags

anyObject = Object (Unique "Anything" "Anything")

setLit :: String
setLit = "Set"

isSet :: Node -> Bool
isSet (Collection _ tags _) = setLit `elem` tags
isSet _ = P.False

anyCollection :: Node
anyCollection = Collection (FormOf N.True) [] (Unique "AnyCollection" "AnyCollection")

anySet :: Node
anySet = Collection (FormOf N.True) [setLit] (Unique "AnySet" "AnySet")

{-| returns a statement actually -}
isSubsetOf :: Node -> Node -> PContext Node
isSubsetOf a b = do
  id <- getNewId
  let subsetRel = Relation a b orderedRel (Unique "subset" id)
  return subsetRel

{-| returns a statement actually -}
isIn :: Node -> Node -> PContext Node
isIn x set = do
  id <- getNewId 
  let isInRel = Relation x set [] (Unique "isIn" id) 
  return isInRel

getElement :: Node -> String -> PContext Node
getElement set@(Collection def tags i) name = do
  newId <- getNewId 
  let e = case def of
        Multiple (x : xs) -> x
        FormOf node -> do
          let obj = Object (Unique name newId)
          addNewStatementM (PContext Node)
        _ -> error "Set.getElement: empty set"
  addNewStatementM (e `isIn` set)
  return e
getElement _ _ = error "Set.getElement: not a set"

empty :: Node
empty = Collection (Multiple []) [setLit] (Unique "Empty" "Empty")

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

getNewSet :: String -> [String] -> ElementTemplate -> PContext Node
getNewSet name tags setType = do
  id <- getNewId 
  let coll = Collection setType (setLit : tags) (Unique name id)
  return coll
