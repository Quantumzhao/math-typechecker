{-# LANGUAGE ScopedTypeVariables #-}
module Set where

import Prelude
import Common
import GHC.Natural
--import System.Random hiding (Finite)
--import Data.UUID
import Control.Monad.State.Lazy
--import Control.Monad.HT
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
  -- a user defined name
  name :: String,
  -- the part before qualifier in classical set notation
  -- a list of variable indices, can only be a scalar or a tuple
  -- e.g. [1, 3, 4] should be interpreted as { (e1, e3, e4) | ... }
  -- e.g. [1] is just { e1 | ... }
  expression :: [Int],
  -- the number of variables in this scope
  variables :: Int,
  -- the set property and also qualifiers (implicitly stated)
  property :: BooleanExpression (SetProperty Int)
  --cardinal :: Cardinality
} deriving (Show)

-- property name, variable name
type SetProperty a = (String, a)

-- should use set properties to infer cardinality first, if failed then fallback to this field
data Cardinality = 
    Finite Finite
  | Infinite Infinite
  | AnyFinite
  | AnyInfinite
  | Any

intersect :: Set -> Set -> Set
intersect a b
  | length (expression a) == length (expression b) =
    let offset = variables a in
    -- let c = case (cardinal a, cardinal b) of
    --         (Finite (Size f1), Finite (Size f2)) -> Finite $ Range 0 (min f1 f2)
    --         (Finite (Size f1), Finite (Range s e)) -> Finite $ Range 0 (min f1 e)
    --         (Finite (Size f1), cb) -> cb
    --         (Finite (Range s e), Finite (Size f1)) -> Finite $ Range 0 (min f1 e)
    --         (Finite (Range s1 e1), Finite (Range s2 e2)) -> Finite $ Range 0 (min e1 e2)
    --         (AnyFinite, _) -> AnyFinite
    --         (ca, _) -> ca
    -- in
    Set (name a ++ " ∩ " ++ name b) 
        (expression a) 
        (variables a + variables b) 
        (And (property a) (property b)) --c
  | otherwise = empty

-- union :: Set -> Set -> Set
-- union a b = Set (name a ++ " ∪ " ++ name b) (From [pa, pb] Or)

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
    Set (name a ++ " - " ++ name b) (expression a) (variables a) 
      (And (property a) $ Not $ rename (variables a) $ property b) --c
  | otherwise = a

-- >>> evalState [] (getSimpleSet "A")
-- >>> 
-- Couldn't match type: [a0]
--                with: StateT (State Labels Set) Identity a
-- Expected: State (State Labels Set) a
--   Actual: [a0]

-- "e" only exists in the local scope, 
-- no need to dynamically generate names for it
getSimpleSet :: String -> State Labels Set
getSimpleSet name = do 
  name' <- genLabel name
  pName <- genLabel $ "p" ++ name
  return $ Set name' [1] 1 $ Characteristic (pName, 1) --Any

-- { e | ∀ e ∈ U, P(e) } where P(x) = True ∀ x ∈ U
universal :: Set
universal = Set "U" [1] 1 ExpTrue --Infinite Uncountable

-- { e | ∀ e ∈ U, P(e) } where P(x) = False ∀ x ∈ U
empty :: Set
empty = Set "∅" [1] 1 ExpFalse -- Finite $ Size 0

-- supply a new name and additional property for the subset
-- thus for any S = { e | P(e) }, its subset will be S' = { e | P(e) ∧ P'(e) }
subset :: Set -> String -> BooleanExpression (SetProperty Int) -> State Labels Set
subset set name p = do 
  name' <- genLabel name
  let newP = And (property set) (rename (variables set) p)
  return $ Set name' [1] (variables set) newP --(cardinal set)

cross :: [Set] -> Set
cross [] = empty
cross [x] = x
cross (x : xs) = 
  let rest = cross xs in
  let offset= length $ expression rest in
  Set 
    (name x ++ " × " ++ name rest) 
    (expression rest ++ fmap (+ offset) (expression x)) 
    (variables x + variables rest) 
    (And (property rest) (rename (variables rest) (property x)))

rename :: Int -> BooleanExpression (SetProperty Int) -> BooleanExpression (SetProperty Int)
rename incr ExpFalse = ExpFalse
rename incr ExpTrue = ExpTrue
rename incr (Characteristic (p, v)) = Characteristic (p, v + incr)
rename incr (Not inner) = Not $ rename incr inner
rename incr (And e1 e2) = And (rename incr e1) (rename incr e2)
rename incr (Or e1 e2) = Or (rename incr e1) (rename incr e2)
rename incr (Xor e1 e2) = Xor (rename incr e1) (rename incr e2)

