module Mapping where

import Node
import ContextState
import Control.Monad.State.Lazy
import Set
import Tags
import Relation
import Control.Monad.Except
import Util

-- compose :: Mapping -> Mapping -> Maybe Mapping
-- compose (Map da ia ba) (Map db ib bb)
--   | ia == db = Just $ Map da ib (getBijectivity ba bb)
--   | otherwise = Nothing
--   where
--     getBijectivity any Bijective = any
--     getBijectivity Bijective any = any
--     getBijectivity Surjective Injective = undefined
--     getBijectivity Injective Surjective = undefined 
--     getBijectivity _ _ = None

-- setOfMappings :: Node
-- setOfMappings = Set (Type (Unary Definition universal universal [])) []


-- newMapping :: String -> Node -> Node -> [String] -> Context
-- newMapping name from to tags = do
--   let mapping = Unary Definition from to tags
--   addNewNode name mapping
--   return ()

-- short for cross, but yields an anonymous set
-- (<.>) :: Node -> Node -> PContext Node
-- (<.>) n1 n2 = do
--   newId <- getNewId
--   let res = DirectProduct (n1, n2) (Exist "cross" newId)
--   return res

applyArg :: Node -> Node -> PContext Node
applyArg (Mapping domain range tags i) arg = do
  isSubset <- arg `isSubsetOfB` domain
  isInFlag <- arg `isInB` domain
  if isSubset then return range
  else if isInFlag then do
    id <- getNewId
    let o = Object (Exist (toLower (nameOf $ key range)) id)
    addNewStatementM $ o `isIn` range
    return o
  else throwError "applyArg: arg is not related to domain"
applyArg _ _ = throwError "applyArg: not a mapping"

