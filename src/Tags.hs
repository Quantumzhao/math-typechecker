module Tags where

reflexiveTag = "reflexive"
transitiveTag = "transitive"
symmetricTag = "symmetric"
antisymmetricTag = "antisymmetric"

-- it should be replaced by injective and surjective in the AST
bijectiveTag = "bijective"
surjectiveTag = "surjective"
injectiveTag = "injective"

finiteTag = "finite"
countableTag = "countable"

equivalenceTag = eqClass
equivalenceRel = [reflexiveTag, transitiveTag, symmetricTag]

orderedRel = [reflexiveTag, transitiveTag, antisymmetricTag]

eqClass = "equivalence"

infixTag = "_infix"

atomTag = "_atom"

classTags = [
    finiteTag,
    countableTag,
    atomTag
  ]

mappingTags = [
    surjectiveTag,
    injectiveTag,
    bijectiveTag
  ]

relationTags = [
    reflexiveTag,
    transitiveTag,
    symmetricTag,
    antisymmetricTag,
    equivalenceTag,
    infixTag
  ]

elems :: [String] -> [String] -> Bool
elems tags list = all (`elem` list) tags
