module Tags where

import Data.Set as Set

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

set :: String
set = "Set"

joinWith :: [String] -> [String] -> [String]
joinWith t1 t2 = Set.toList $ Set.fromList (t1 ++ t2)
