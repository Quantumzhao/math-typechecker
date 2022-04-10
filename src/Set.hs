module Set where

import Prelude
import Common
import GHC.Natural
import System.Random hiding (Finite)
import Data.UUID
import Control.Monad.State.Lazy
import Control.Monad.HT
--import Control.Monad.Random.Strict

-- p: property
-- t: tag
-- Note in this set representation, the expression part is just 
--    the identity function. It should be the same as in classical notation: 
-- let the set B be { f(x) | ∀ x ∈ A, P(A) } where the expression is f: A ↦ B
-- Thus it is equivalent to { x | ∀ x ∈ A, f(P(A)) }
-- If we let P2 = f ∘ P, then it can be further contracted to the notation used in here
-- The variables (e.g. a, b, ...) and properties are in lists to facillitate multiple 
--    independent qualifiers
data Set = Set {
  name :: String,
  expression :: [String],
  variables :: [String],
  property :: BooleanExpression (SetProperty String)
  --cardinal :: Cardinality
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

relCompl :: Set -> Set -> Set
relCompl a b
  -- let c = case cardinal a of
  --         Finite (Size f1) -> 
  --           let rangeFrom = case cardinal b of
  --                           (Finite (Size f2)) -> f1 >- f2
  --                           (Finite (Range r1 r2)) -> f1 >- r2
  --                           _ -> 0
  --           in Finite $ Range rangeFrom f1
  --         Finite (Range r1' r2') -> 
  --           let rangeFrom = case cardinal b of
  --                           (Finite (Size f2)) -> r1' >- f2
  --                           (Finite (Range r1 r2)) -> r1' >- r2
  --                           _ -> 0
  --           in Finite $ Range rangeFrom r1'
  --         ca -> ca
  -- in
  | expression a == expression b && variables a == variables b = 
    Set (name a ++ " - " ++ name b) (expression a) (variables a) (And (property a) $ Not $ property b) --c
  | otherwise = a

-- "e" only exists in the local scope, 
-- no need to dynamically generate names for it
getSimpleSet :: String -> State Labels Set
getSimpleSet name = do
  name' <- genLabel name
  pName <- genLabel $ "p" ++ name
  return $ Set name' stdVars stdVars $ Characteristic (pName, "e") --Any

-- { e | ∀ e ∈ U, P(e) } where P(x) = True ∀ x ∈ U
universal :: Set
universal = Set "U" stdVars stdVars ExpTrue --Infinite Uncountable

-- { e | ∀ e ∈ U, P(e) } where P(x) = False ∀ x ∈ U
empty :: Set
empty = Set "∅" stdVars stdVars ExpFalse -- Finite $ Size 0

-- supply a new name and additional property for the subset
-- thus for any S = { e | P(e) }, its subset will be S' = { e | P(e) ∧ P'(e) }
subset :: Set -> String -> BooleanExpression (SetProperty String) -> State Labels Set
subset set name p = do 
  name' <- genLabel name
  let newP = And (property set) p
  return $ Set name' stdVars (variables set) newP --(cardinal set)

stdVars :: [String]
stdVars = ["e"]

cross :: [Set] -> Set
cross [] = empty
cross [x] = x
cross (x : xs) = 
  let rest = cross xs in
  Set 
    (name x ++ " × " ++ name rest) 
    (expression x ++ expression rest) 
    (variables x ++ variables rest) 
    (And (property x) (property rest))
