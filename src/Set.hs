{-# LANGUAGE ScopedTypeVariables #-}
module Set where

import Prelude
import Common
import GHC.Natural
--import System.Random hiding (Finite)
--import Data.UUID
import Control.Monad.State.Lazy
import qualified Data.List as List
import Data.List (intersperse, intercalate)
import qualified Data.Set as DSet
import ContextState
import Node
--import Control.Monad.HT
--import Control.Monad.Random.Strict

-- -- p: property
-- -- t: tag
-- -- Note in this set representation, the expression part is just 
-- --    the identity function. It should be the same as in classical notation: 
-- -- let the set B be { f(x) | ∀ x ∈ A, P(A) } where the expression is f: A ↦ B
-- -- Thus it is equivalent to { x | ∀ x ∈ A, f(P(A)) }
-- -- If we let P2 = f ∘ P, then it can be further contracted to the notation used in here
-- -- The variables (e.g. a, b, ...) and properties are in lists to facillitate multiple 
-- --    independent qualifiers
-- data Set = Set {
  -- a user defined name
  -- name :: String,
  -- structure :: SetExpression
  -- -- the part before qualifier in classical set notation
  -- -- a list of variable indices, can only be a scalar or a tuple
  -- -- e.g. [1, 3, 4] should be interpreted as { (e1, e3, e4) | ... }
  -- -- e.g. [1] is just { e1 | ... }
  -- expression :: [Int],
  -- -- the number of variables in this scope
  -- variables :: Int,
  -- -- the set property and also qualifiers (implicitly stated)
  -- property :: BooleanExpression (SetProperty Int)
  -- --cardinal :: Cardinality
-- } deriving (Show, Eq)

-- data SetExpression
--   = Symbol String
--   | SetNode Set
--   | Intersection [SetExpression]
--   | Union [SetExpression]
--   | Complement SetExpression SetExpression
--   | Subset SetExpression
--   | Superset SetExpression
--   | Cross [SetExpression]
--   deriving (Show, Eq)

-- -- property name, variable name
-- type SetProperty a = (String, a)

-- -- should use set properties to infer cardinality first, if failed then fallback to this field
-- data Cardinality =
--     Finite Finite
--   | Infinite Infinite
--   | AnyFinite
--   | AnyInfinite
--   | Any

-- assocBinOp :: ([SetExpression] -> SetExpression) -> String -> [Set] -> Set
-- assocBinOp binop notation sets = 
--   Set (intercalate notation (fmap name sets)) $ binop (fmap SetNode sets)

-- intersect :: [Set] -> State Int Set
-- intersect [] = return empty
-- intersect [x] = return x
-- intersect (x : xs) = do
--   rest@(Set n' e' v' p') <- intersect xs
--   if rest == empty || x == empty then return empty
--   else if length (expression x) == length e' then do
--     -- let c = case (cardinal a, cardinal b) of
--     --         (Finite (Size f1), Finite (Size f2)) -> Finite $ Range 0 (min f1 f2)
--     --         (Finite (Size f1), Finite (Range s e)) -> Finite $ Range 0 (min f1 e)
--     --         (Finite (Size f1), cb) -> cb
--     --         (Finite (Range s e), Finite (Size f1)) -> Finite $ Range 0 (min f1 e)
--     --         (Finite (Range s1 e1), Finite (Range s2 e2)) -> Finite $ Range 0 (min e1 e2)
--     --         (AnyFinite, _) -> AnyFinite
--     --         (ca, _) -> ca
--     -- in
--       (Set xn xe xv xp) <- rename v' e' x
--       return $ Set (xn ++ " ∩ " ++ n') xe (xv + v') (And xp p') --c
--   else return empty

-- intersect :: [Set] -> Set
-- intersect = assocBinOp Intersection " ∩ "

-- union :: [Set] -> Set
-- union = assocBinOp Union " ∪ "

-- cross :: [Set] -> Set
-- cross = assocBinOp Cross " × "

-- complement :: Set -> Set -> Set
-- complement a b = Set (name a ++ " - " ++ name b) $ Complement (structure a) (structure b) 

-- subset :: SetExpression -> String -> Set
-- subset set subName = Set subName $ Superset set

intersect :: Node -> Node -> GraphState
intersect a@(Set an at) b@(Set bn bt) = do
  (Context nodes count) <- get
  let newName = an ++ " ∩ " ++ bn
  let newNode = Set newName (at ++ bt)
  addNewNode newNode
  addNewNode (BinOp a b Intersect)
  addNewNode (Relation newNode a Subset [])
  addNewNode (Relation newNode b Subset [])
  return ()
intersect _ _ = return ()

newNameFrom :: [Node] -> String -> String
newNameFrom sets binop = intercalate binop (fmap name sets)

-- union :: [Set] -> State Int Set
-- union [] = return empty
-- union [x] = return x
-- union (x : xs) = do
--   (Set n' e' v' p') <- union xs
--   (Set xn xe xv xp) <- rename v' e' x
--   let newN = xn ++ " ∪ " ++ n'
--   if length e' == 1 && length xe == 1 then
--     return $ Set newN xe (xv + v') (Or xp p')
--   else if length e' == 2 && length xe == 2 then do
--     put v'
--     newP <- unionOn2_tuple p' xp
--     return $ Set newN xe (xv + v') newP
--   else return empty
--   -- rest <- union xs
--   -- if rest == empty then return x
--   -- else if x == empty then return rest
--   -- else if length (expression x) > length (expression rest) then return x
--   -- else if length (expression x) < length (expression rest) then return rest
--   -- else do

--     -- let offset = variables rest in
--     -- Set (name x ++ " ∪ " ++ name rest)
--     --     (expression x)
--     --     (variables x + variables rest)
--     --     ExpFalse 
--     where
--       unionOn2_tuple (TupleAnd a c) tupB = do
--         acc <- (get :: State Int Int)
--         let (TupleAnd b d) = renameP (+ acc) tupB
--         return $ ((a `minus` b) `And` c) `Or`
--                  ((a `And` b) `And` (c `Or` d)) `Or`
--                  ((b `minus` a) `And` d)
--       -- code should never take this path
--       unionOn2_tuple _ _ = return ExpFalse
--       minus a b = And a (Not b)
--     --   unionOnn_tuple (TupleAnd a a') (TupleAnd b d') = undefined 
-- -- Set (name a ++ " ∪ " ++ name b) (From [pa, pb] Or)

-- relCompl :: Set -> Set -> Set
-- relCompl a b
--   -- let c = case cardinal a of
--   --         Finite (Size f1) -> 
--   --           let rangeFrom = case cardinal b of
--   --                           (Finite (Size f2)) -> f1 >- f2
--   --                           (Finite (Range r1 r2)) -> f1 >- r2
--   --                           _ -> 0
--   --           in Finite $ Range rangeFrom f1
--   --         Finite (Range r1' r2') -> 
--   --           let rangeFrom = case cardinal b of
--   --                           (Finite (Size f2)) -> r1' >- f2
--   --                           (Finite (Range r1 r2)) -> r1' >- r2
--   --                           _ -> 0
--   --           in Finite $ Range rangeFrom r1'
--   --         ca -> ca
--   -- in
--   | expression a == expression b && variables a == variables b =
--     Set (name a ++ " - " ++ name b) (expression a) (variables a)
--       (And (property a) $ Not $ renameP (+ variables a) $ property b) --c
--   | otherwise = a

-- -- >>> evalState [] (getSimpleSet "A")
-- -- >>> 
-- -- Couldn't match type: [a0]
-- --                with: StateT (State Labels Set) Identity a
-- -- Expected: State (State Labels Set) a
-- --   Actual: [a0]

-- -- "e" only exists in the local scope, 
-- -- no need to dynamically generate names for it
-- getSimpleSet :: String -> State Labels Set
-- getSimpleSet name = do
--   name' <- genLabel name
--   pName <- genLabel $ "p" ++ name
--   return $ Set name' [1] 1 $ Characteristic (pName, 1) --Any

-- -- { e | ∀ e ∈ U, P(e) } where P(x) = True ∀ x ∈ U
-- universal :: Set
-- universal = Set "U" [1] 1 ExpTrue --Infinite Uncountable
-- universal = Set "U" (Symbol "U")

-- -- { e | ∀ e ∈ U, P(e) } where P(x) = False ∀ x ∈ U
-- empty :: Set
-- empty = Set "∅" [1] 1 ExpFalse -- Finite $ Size 0
-- empty = Set "∅" (Symbol "∅")

-- -- supply a new name and additional property for the subset
-- -- thus for any S = { e | P(e) }, its subset will be S' = { e | P(e) ∧ P'(e) }
-- subset :: Set -> String -> BooleanExpression (SetProperty Int) -> State Labels Set
-- subset set name p = do
--   name' <- genLabel name
--   let newP = And (property set) (renameP (+ variables set) p)
--   return $ Set name' [1] (variables set) newP --(cardinal set)

-- cross :: [Set] -> State Int Set
-- cross [] = return empty
-- cross [x] = return x
-- cross (x : xs) = do
--   (Set n' e' v' p') <- cross xs
--   (Set xn xe xv xp) <- offsetVars v' x
--   return $ Set (xn ++ " × " ++ n') (e' ++ xe) (v' + xv) (TupleAnd p' xp)

-- renameP :: (Int -> Int) -> BooleanExpression (SetProperty Int) -> BooleanExpression (SetProperty Int)
-- renameP f ExpFalse = ExpFalse
-- renameP f ExpTrue = ExpTrue
-- renameP f (Characteristic (p, v)) = Characteristic (p, f v)
-- renameP f (Not inner) = Not $ renameP f inner
-- renameP f (And e1 e2) = And (renameP f e1) (renameP f e2)
-- renameP f (Or e1 e2) = Or (renameP f e1) (renameP f e2)
-- renameP f (Xor e1 e2) = Xor (renameP f e1) (renameP f e2)
-- renameP f (TupleAnd e1 e2) = TupleAnd (renameP f e1) (renameP f e2)

-- -- e.g. change { x₁x₂ | P₁(x₁) ∧ P₂(x₂) } to { x₃x₄ | P₁(x₃) ∧ P₂(x₄) }
-- --     so that when doing binary set operations, the variable names don't clash
-- -- of course the notation used in this program would be 
-- offsetVars :: Int -> Set -> State Int Set
-- offsetVars index (Set sn se sv sp) = do
--   acc <- get
--   let renameOrIncr v = if v == index then index else v + acc
--   let set' = Set sn (fmap renameOrIncr se) sv (renameP renameOrIncr sp)
--   modify (+ sv)
--   return set'

-- -- e.g. change { x₁x₂ | P₁(x₁) ∧ P₂(x₂) } and { x₃x₄ | P₃(x₃) ∧ P₄(x₄) }
-- --     to { x₃x₄ | P(x₃) ∧ P(x₄) } and { x₃x₄ | P(x₃) ∧ P(x₄) }
-- --     so that we can properly apply intersection, union ... in the future
-- --     which would then be { x₃x₄ | P(x₃) ∧ P(x₄) }
-- replaceVars :: [Int] ->Set ->  Set
-- replaceVars (i : is) s@(Set sn (se : ses) sv sp) =
--   let sp' = renameP (\v -> if v == se then i else v) sp in
--   let (Set _ ses' _ sp'') = replaceVars is (Set sn ses sv sp') in
--   Set sn (se : ses') sv sp''
-- replaceVars _ set = set

-- -- replace and then offset variable indices
-- rename :: Int -> [Int] -> Set -> State Int Set
-- rename offset indices s = do
--   s' <- offsetVars offset s
--   return $ replaceVars indices s'

-- superSet :: Set -> Set
-- superSet set = undefined

-- setEqual :: Set -> Set -> Bool
-- setEqual = undefined 

-- isIn :: String -> Set -> Bool
-- isIn v set = undefined 

-- isProperSubset :: Set -> Set -> Bool
-- isProperSubset sub sup = isSubset sub sup && setEqual sub sup 

-- isSubset :: Set -> Set -> Bool
-- isSubset = undefined 


