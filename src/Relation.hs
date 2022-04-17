module Relation where
import Node
import ContextState
import Set
import Control.Monad.State.Lazy

relationLit :: String
relationLit = "Relation"

relTags :: [String] -> [String]
relTags tags = "Relation" : tags

genRelation :: Node -> Node -> [String] -> Node
genRelation relFrom relTo tags = Relation relFrom relTo (relTags tags)

inFnDef :: Node
inFnDef = Relation universal universal []

isIn :: Node -> Node -> Node
isIn e set = Statement Definition []

reflexiveTag :: String
reflexiveTag = "reflexive"
transitiveTag :: String
transitiveTag = "transitive"
symmetricTag :: String
symmetricTag = "symmetric"
antisymmetricTag :: String
antisymmetricTag = "antisymmetric"
