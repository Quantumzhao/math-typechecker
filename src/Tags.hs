module Tags where

reflexiveTag = "reflexive"
transitiveTag = "transitive"
symmetricTag = "symmetric"
antisymmetricTag = "antisymmetric"

-- no bijective tag because it should be replaced by injective and surjective in the AST
surjectiveTag = "surjective"
injectiveTag = "injective"

finiteTag = "finite"
countableTag = "countable"

equivalenceRel = [reflexiveTag, transitiveTag, symmetricTag]

orderedRel = [reflexiveTag, transitiveTag, antisymmetricTag]

eqClass = "equivalence"

classTags = [
    finiteTag,
    countableTag
  ]

mappingTags = [
    surjectiveTag,
    injectiveTag
  ]

elems :: [String] -> [String] -> Bool
elems tags list = all (`elem` list) tags
