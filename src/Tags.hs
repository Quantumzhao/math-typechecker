module Tags where

reflexiveTag :: String
reflexiveTag = "reflexive"
transitiveTag :: String
transitiveTag = "transitive"
symmetricTag :: String
symmetricTag = "symmetric"
antisymmetricTag :: String
antisymmetricTag = "antisymmetric"

equivalenceRel :: [String]
equivalenceRel = [reflexiveTag, transitiveTag, symmetricTag]

orderedRel :: [String]
orderedRel = [reflexiveTag, transitiveTag, antisymmetricTag]

eqClass :: String
eqClass = "equivalence"

