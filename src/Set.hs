{-# LANGUAGE ScopedTypeVariables #-}

module Set where

import Prelude
import Common
import GHC.Natural
import Control.Monad.State.Lazy
import qualified Data.List as List
import Data.List (intersperse, intercalate)
import ContextState
import Node
import Data.Map
import Tags

setLit = "Set"

isSet :: Node -> Bool
isSet (Collection _ tags _) = setLit `elem` tags
isSet _ = False


-- unionFnDef :: Node
-- unionFnDef = Mapping setOfDirectProducts universal [] "union" 0

-- union :: Node -> Node -> PContext Node
-- union a b
--   | isSet a && isSet b = do
--     id <- getNewId 
--     ab <- cross a b
--     application <- substitute unionFnDef ab
--     let newName = nameOf a ++ " union " ++ nameOf b
--     let newNode = Set (FormOf application) [] newName id
--     addNewStatement (a `isSubsetOf` newNode)
--     addNewStatement (b `isSubsetOf` newNode)
--     return newNode
--   | otherwise = error "union: not sets"

-- complementFnDef :: Node
-- complementFnDef = Mapping setOfDirectProducts universal [] " minus " 0

-- complement :: Node -> Node -> PContext Node
-- complement a b
--   | isSet a && isSet b = do
--     let newName = nameOf a ++ " - " ++ nameOf b
--     id <- getNewId 
--     ab <- cross a b
--     application <- substitute complementFnDef ab
--     let newNode = Set (FormOf application) (tags a) newName id
--     addNewStatement (newNode `isSubsetOf` a)
--     return newNode
--   | otherwise = error "complement: not sets"

-- subsetFnDef :: Node
-- subsetFnDef = Relation universal universal [] "subset" 0

-- subset :: Node -> PContext Node
-- subset a@(Collection def tags nameA _) = do
--     (nodes, idGen) <- get
--     newId <- getNewId 
--     let newNode = Set def tags nameA newId
--     let newStatement = newNode `isSubsetOf` a
--     addNewStatement newStatement
--     return newNode
-- subset _ = error "Set.subset: not a set"

{-| returns a statement actually -}
-- isSubsetOf :: Node -> Node -> Node
-- isSubsetOf a b = Relation a b orderedRel "subset" 0

-- isSubsetOf' :: Node -> Node -> PContext Bool
-- isSubsetOf' a b = do
--   graph <- getNodes
--   case findFirst (== a `isSubsetOf` b) graph of
--     Just _ -> return True
--     Nothing -> return False

-- getElement :: Node -> PContext Node
-- getElement set@(Collection def tags name id) = do
--   newId <- getNewId 
--   let e = case def of
--         Collection (x : xs) -> x
--         FormOf node -> node
--         Universal -> Object (show newId) newId
--         _ -> error "Set.getElement: empty set"
--   addNewStatement (e `isIn` set)
--   return e
-- getElement _ = error "Set.getElement: not a set"

{-| returns a statement actually -}
-- isIn :: Node -> Node -> Node
-- isIn x set = Relation x set [] anonymous 0

-- isIn' :: Node -> Node -> PContext Bool
-- isIn' e set = do
--   nodes <- getNodes 
--   case findFirst (== e `isIn` set) nodes of
--     Just _ -> return True
--     Nothing -> return False

-- universal :: Node
-- universal = Set Universal ["Universal"] "Universal" 0

-- empty :: Node
-- empty = Collection Empty ["Empty"] "Empty" 0

-- setOfDirectProducts :: Node
-- setOfDirectProducts = Collection (FormOf $ Tuple (universal, universal) anonymous 0) [] "DirectProducts" 0

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

-- getNewSet :: String -> [String] -> PContext Node
-- getNewSet name tags = do
--   newSet <- subset universal
--   addNewStatement (newSet `isSubsetOf` universal)
--   return newSet

-- substitute :: Node -> Node -> PContext Node
-- substitute x binop@(Mapping dom ran tags name id) = do
--   isSubset <- x `isSubsetOf'` dom
--   isElem <- x `isIn'` dom
--   newId <- getNewId 
--   if isSubset || isElem then return $ Mapping x ran tags name newId
--   else error "Set.substitute: not a subset of domain"
-- substitute x _ = error "Set.substitute: not a mapping"
