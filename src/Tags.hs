module Tags where

reflexiveTag :: String
reflexiveTag = "reflexive"
transitiveTag :: String
transitiveTag = "transitive"
symmetricTag :: String
symmetricTag = "symmetric"
antisymmetricTag :: String
antisymmetricTag = "antisymmetric"

-- no bijective tag because it should be replaced by injective and surjective in the AST
surjectiveTag :: String
surjectiveTag = "surjective"
injectiveTag :: String
injectiveTag = "injective"

equivalenceRel :: [String]
equivalenceRel = [reflexiveTag, transitiveTag, symmetricTag]

orderedRel :: [String]
orderedRel = [reflexiveTag, transitiveTag, antisymmetricTag]

eqClass :: String
eqClass = "equivalence"

elems :: [String] -> [String] -> Bool
elems tags list = all (`elem` list) tags
