module Relation where
import Node
import ContextState
import Set

relationLit :: String
relationLit = "Relation"

relTags :: [String] -> [String]
relTags tags = "Relation" : tags

genRelation :: Node -> Node -> [String] -> Node
genRelation relFrom relTo tags = Relation relFrom relTo (relTags tags)
