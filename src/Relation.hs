{-# LANGUAGE ScopedTypeVariables #-}
module Relation where
import Node
import ContextState
import Set
import Control.Monad.State.Lazy
import Tags

-- relationLit :: String
-- relationLit = "Relation"

-- relTags :: [String] -> [String]
-- relTags tags = "Relation" : tags

-- genRelation :: Node -> Node -> [String] -> Node
-- genRelation relFrom relTo tags = Relation relFrom relTo (relTags tags)

-- applyR (Relation dom cod tags name _) b = do
--   nodes <- getNodes
--   if elems equivalenceRel tags then do
--     newId <- getNewId 
--     newId' <- getNewId
--     let newNode :: Node = Set (FormOf $ Relation dom b tags name newId) [eqClass] ("eq " ++ nameOf dom) newId'
--     addNewStatement (newNode `isSubsetOf` dom)
--     return eqClass
--   else undefined
-- applyR _ _ = error "Relation.applyR: not a relation"

-- -- a ~ ?
-- applyL (Relation dom cod tags name _) a = do
-- applyL _ a = error "Relation.applyL: not a relation"

elems :: [String] -> [String] -> Bool
elems tags list = all (`elem` list) tags