module Mapping where

import Node
import ContextState
import Control.Monad.State.Lazy
import Set
-- data Mapping = Map Node Node Bijectivity

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

newMapping :: String -> Node -> Node -> [String] -> Context
newMapping name from to tags = do
  let mapping = Unary Definition from to tags
  addNewNode name mapping

compose :: String -> Node -> Node -> Context
compose name (Unary ff fDomain fRange ft) (Unary gf gDomain gRange gt) = do 
  (nodes, _) <- get
  isSubset <- fRange `isSubsetOf'` gDomain
  if isSubset then do
    let newMap = Unary Definition fDomain gRange []
    addNewNode name newMap 
  else error "compose: domain and range mismatch"
compose _ _ _ = error "compose: not mappings"
