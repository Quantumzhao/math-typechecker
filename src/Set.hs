module Set where

import Common
import GHC.Natural
import System.Random hiding (Finite)
import Data.UUID
import Control.Monad.State.Lazy
--import Control.Monad.Random.Strict

-- p: property
-- t: tag
data Set = Set {
  name :: String,
  qualifier :: [String],
  property :: BooleanExpression (SetProperty String),
  cardinal :: Cardinality
}

type SetProperty a = (String, a)

-- should use set properties to infer cardinality first, if failed then fallback to this field
data Cardinality = 
    Finite Finite
  | Infinite Infinite
  | AnyFinite
  | AnyInfinite
  | Any

-- intersect :: Set -> Set -> Set
-- intersect a b = 
--   let c = case (cardinal a, cardinal b) of
--           (Finite (Size f1), Finite (Size f2)) -> Finite $ Range 0 (min f1 f2)
--           (Finite (Size f1), Finite (Range s e)) -> Finite $ Range 0 (min f1 e)
--           (Finite (Size f1), cb) -> cb
--           (Finite (Range s e), Finite (Size f1)) -> Finite $ Range 0 (min f1 e)
--           (Finite (Range s1 e1), Finite (Range s2 e2)) -> Finite $ Range 0 (min e1 e2)
--           (AnyFinite, _) -> AnyFinite
--           (ca, _) -> ca
--   in
--   Set (name a ++ " ∩ " ++ name b) ["a"] (From [property a, property b] And) c

-- union :: Set -> Set -> Set
-- union sa@(Set na pa ca) sb@(Set nb pb cb) = 
--   Set (na ++ " ∪ " ++ nb) (From [pa, pb] Or)

-- relCompl :: Set -> Set -> Set
-- relCompl sa@(Set na pa ca) sb@(Set nb pb cb) = 
--   let c = case ca of
--           Finite (Size f1) -> 
--             let rangeFrom = case cb of
--                             (Finite (Size f2)) -> f1 >- f2
--                             (Finite (Range r1 r2)) -> f1 >- r2
--                             _ -> 0
--             in Finite $ Range rangeFrom f1
--           Finite (Range r1' r2') -> 
--             let rangeFrom = case cb of
--                             (Finite (Size f2)) -> r1' >- f2
--                             (Finite (Range r1 r2)) -> r1' >- r2
--                             _ -> 0
--             in Finite $ Range rangeFrom r1'
--           _ -> ca
--   in
--   Set (na ++ " - " ++ nb) (From [pa, From [pb] Not] And) c
getSimpleSet :: String -> Set
getSimpleSet name = Set name ["a"] (Characteristic ("P", "a")) Any

-- universal :: Set
-- universal = Set "U" (Symbol "Universal") $ Infinite Uncountable

-- empty :: Set
-- empty = Set "∅" (Symbol "Empty") $ Finite $ Size 0

-- -- supply a new name and additional property for the subset
-- -- thus for any S = { e | P(e) }, its subset will be S' = { e | P(e) ∧ P'(e) }
-- subset :: Set -> String -> SetProperty -> Set
-- subset set str p = Set str $ From [property set, p] And

-- cross :: [Set] -> Set
-- cross [] = empty
-- cross [x] = x
-- cross (x : xs) = 
--   let rest = cross xs in
--   Set (name x ++ " × " ++ name rest) 
