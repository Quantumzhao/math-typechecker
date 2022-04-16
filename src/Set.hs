{-# LANGUAGE ScopedTypeVariables #-}

module Set where

import Prelude
import Common
import GHC.Natural
import Control.Monad.State.Lazy
import qualified Data.List as List
import Data.List (intersperse, intercalate)
import ContextState
import Node

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

-- no puns intended
-- setTags :: [String] -> [String]
-- setTags tags = if setLit `elem` tags then tags else setLit : tags

isSet :: Node -> Bool
isSet (Set {}) = True
isSet _ = False

intersectFnDef :: Node
intersectFnDef = Binary universal universal universal []

intersect :: Record -> Record -> Context
intersect (nameA, a) (nameB, b)
  | isSet a && isSet b = do
    (nodes, _) <- get
    let apply' = apply nodes
    let application = apply' (apply' intersectFnDef a) b
    let newName = nameA ++ " ∩ " ++ nameB
    let newNode = Set (Type application) (tags a ++ tags b)
    addNewNode newName newNode
    addNewStatement (newNode `isSubsetOf` a)
    addNewStatement (newNode `isSubsetOf` b)
  | otherwise = return ()

-- unionFnDef :: Node
-- unionFnDef = Binary universal universal universal []

-- union :: Record -> Record -> Context
-- union (nameA, a) (nameB, b)
--   | isSet a && isSet b = do
--     let partialUnion = Unary (App unionFnDef a) universal universal []
--     let unionApp = App partialUnion b
--     let newName = nameA ++ " ∪ " ++ nameB
--     let newNode = Object unionApp (setTags [])
--     addNewNode newName newNode
--     addNewStatement (a `isSubsetOf` newNode)
--     addNewStatement (b `isSubsetOf` newNode)
--   | otherwise = return ()

-- crossFnDef :: Node
-- crossFnDef = Binary universal universal setOfDirectProducts []

-- cross :: Record -> Record -> Context
-- cross (nameA, a) (nameB, b)
--   | isSet a && isSet b = do
--     let partialCross = Unary (App crossFnDef a) universal setOfDirectProducts []
--     let newName = nameA ++ " × " ++ nameB
--     let newNode = Object (App partialCross b) (setTags [])
--     addNewNode newName newNode
--   | otherwise = return ()

-- complementFnDef :: Node
-- complementFnDef = Binary universal universal universal []

-- complement :: Record -> Record -> Context
-- complement (nameA, a) (nameB, b)
--   | isSet a && isSet b = do
--     let newName = nameA ++ " - " ++ nameB
--     let partialCompl = Unary (App complementFnDef a) universal universal []
--     let newNode = Object (App partialCompl b) (tags a)
--     addNewNode newName newNode
--     addNewStatement (newNode `isSubsetOf` a)
--   | otherwise = return ()
-- complement :: Node -> Node -> Context 
-- complement a@(Set at) b@(Set bt) = do
--   nodes <- get
--   let newName = an ++ " - " ++ bn
--   let newNode = Set newName at
--   addNewNode newNode
--   addNewNode (BinOp a b RelCompl) 
-- complement _ _ = return ()

subsetFnDef :: Node
subsetFnDef = Relation universal universal []

subset :: String -> Record  -> Context
subset name (nameA, a@(Set def tags)) = do
    (nodes, idGen) <- get
    let newId = idGen
    let newNode = Set def tags
    let newStatement = newNode `isSubsetOf` a
    addNewNode name newNode
    addNewStatement newStatement
subset _ _ = return ()

-- powersetFnDef :: Node
-- powersetFnDef = Unary Definition universal universal []
-- powerSet :: Record -> Context
-- powerSet (name, set) 
--   | isSet set = do
--     let newNode = 
--     let newStatement = set `isSubsetOf` newNode
--     addNewNode ("Pow(" ++ name ++ ")") newNode
--     addNewStatement newStatement
--   | otherwise = return ()

{-| returns a statement actually -}
isSubsetOf :: Node -> Node -> Node
isSubsetOf a b =
  let partialApp = App subsetFnDef a in
  let fullApp = App (FixedRelation partialApp universal []) b in
  Statement fullApp []

isSubsetOf' :: Graph -> Node -> Node -> Bool
isSubsetOf' graph a b =
  case findFirst (== a `isSubsetOf` b) graph of
    Just _ -> True
    Nothing -> False

universal :: Node
universal = Set Universal []

empty :: Node
empty = Set Empty []

setOfDirectProducts :: Node
setOfDirectProducts = Set (Type $ Tuple [universal, universal] []) []

-- setOfPowersets :: Node
-- setOfPowersets = Object Definition (setTags ["setOfSets"])

-- newNameFrom :: [Node] -> String -> String
-- newNameFrom sets binop = intercalate binop (fmap name sets)

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

apply :: Graph -> Node -> Node -> Node
apply nodes binop@(Binary l r o ts) v = 
  if isSubsetOf' nodes v l then
    Unary (App binop v) r o ts
  else error "apply binary"
apply nodes un@(Unary u v o ts) v' = 
  if isSubsetOf' nodes v' v then o
  else error "apply unary"
apply nodes rel@(Relation l r ts) v =
  if isSubsetOf' nodes v l then 
    FixedRelation (App rel v) r ts
  else error "apply relation"
apply nodes fixed@(FixedRelation u r ts) v=
  if isSubsetOf' nodes r v then
    Statement (App fixed v) ts
  else error "apply fixed"
apply _ _ _ = error "apply no match" 
