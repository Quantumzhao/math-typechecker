module Relation where
import ContextState
import Node
import Tags

genRelation :: Node -> Node -> [String] -> String -> StateAnd Relation
genRelation d c t n = Relation d c t . Unique n <$> getNewId

-- subsetFnDef :: Relation
-- subsetFnDef = Relation Node Node orderedRel (Unique "subset" "subset")
