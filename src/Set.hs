module Set where

import Common

-- p: property
-- t: tag
data Set = Set {
  name :: String,
  property :: SetProperty
}

data SetProperty = Symbol String | From [SetProperty] BooleanExpression

intersect :: Set -> Set -> Set
intersect sa@(Set na pa) sb@(Set nb pb) = 
  Set (na ++ " ∩ " ++ nb) (From [pa, pb] And)

union :: Set -> Set -> Set
union sa@(Set na pa) sb@(Set nb pb) = 
  Set (na ++ " ∪ " ++ nb) (From [pa, pb] Or)

relCompl :: Set -> Set -> Set
relCompl sa@(Set na pa) sb@(Set nb pb) = 
  Set (na ++ " - " ++ nb) (From [pa, From [pb] Not] And)

universal :: Set
universal = Set "U" (Symbol "Universal")

empty :: Set
empty = Set "∅" (Symbol "Empty")

-- supply a new name and additional property for the subset
-- thus for any S = { e | P(e) }, its subset will be S' = { e | P(e) ∧ P'(e) }
subset :: Set -> String -> SetProperty -> Set
subset set str p = Set str $ From [property set, p] And

-- cross :: [Set] -> Set
-- cross [] = empty
-- cross [x] = x
-- cross (x : xs) = 
--   let rest = cross xs in
--   Set (name x ++ " × " ++ name rest) 