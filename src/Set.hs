module Set where

import Prelude as P
import ContextState ( PContext, addNewNode, getRelations, getNewId, getNodeByName )
import Node as N ( Identifier(Exist), Node(Class, Alias) )
import Control.Monad.Except ( MonadError(throwError) )

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

-- generate a new set
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

-- check if two nodes are the same
isSameAs :: Node -> Node -> Bool
isSameAs a b = trackAlias a == trackAlias b

-- get the original node that it refers to
trackAlias :: Node -> Node
trackAlias (Alias ref _) = trackAlias ref
trackAlias n = n

addUniverse :: PContext ()
addUniverse = do
  id <- getNewId
  let res = Class [] (Exist "Universe" id)
  addNewNode res

addEmpty :: PContext ()
addEmpty = do 
  id <- getNewId
  let res = Class [setLit] (Exist "Empty" id)
  addNewNode res

getUniverse :: PContext Node
getUniverse = getNodeByName "Universe"

getEmpty :: PContext Node
getEmpty = getNodeByName "Empty"
