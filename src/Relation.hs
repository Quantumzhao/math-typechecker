module Relation where
import Node

relationLit :: String
relationLit = "Relation"

relTags :: [String] -> [String]
relTags tags = "Relation" : tags

genRelation :: [String] -> Node
genRelation tags = Object Definition (relTags tags)
