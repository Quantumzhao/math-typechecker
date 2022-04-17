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

isSet :: Node -> Bool
isSet (Set {}) = True
isSet _ = False

intersectFnDef :: Node
intersectFnDef = Binary universal universal universal []

intersect :: Record -> Record -> PContext Record
intersect (nameA, a) (nameB, b)
  | isSet a && isSet b = do
    (nodes, _) <- get
    application <- applyR'ed a intersectFnDef >>= applyR'ed b
    let newName = nameA ++ " ∩ " ++ nameB
    let newNode = Set (Type application) (tags a ++ tags b)
    addNewStatement (newNode `isSubsetOf` a)
    addNewStatement (newNode `isSubsetOf` b)
    return $ record newName newNode
  | otherwise = error "intersect: not sets"

unionFnDef :: Node
unionFnDef = Binary universal universal universal []

union :: Record -> Record -> PContext Record
union (nameA, a) (nameB, b)
  | isSet a && isSet b = do
    (nodes, _) <- get
    application <- applyR'ed a unionFnDef >>= applyR'ed b
    let newName = nameA ++ " ∪ " ++ nameB
    let newNode = Set (Type application) []
    addNewStatement (a `isSubsetOf` newNode)
    addNewStatement (b `isSubsetOf` newNode)
    return $ record newName newNode
  | otherwise = error "union: not sets"

crossFnDef :: Node
crossFnDef = Binary universal universal setOfDirectProducts []

cross :: Record -> Record -> PContext Record
cross (nameA, a) (nameB, b)
  | isSet a && isSet b = do
    (nodes, _) <- get
    application <- applyR'ed a crossFnDef >>= applyR'ed b
    let newName = nameA ++ " × " ++ nameB
    let newNode = Set (Type application) []
    return $ record newName newNode
  | otherwise = error "cross: not sets"

complementFnDef :: Node
complementFnDef = Binary universal universal universal []

complement :: Record -> Record -> PContext Record
complement (nameA, a) (nameB, b)
  | isSet a && isSet b = do
    let newName = nameA ++ " - " ++ nameB
    (nodes, _) <- get
    application <- applyR'ed a crossFnDef >>= applyR'ed b
    let newNode = Set (Type application) (tags a)
    addNewStatement (newNode `isSubsetOf` a)
    return $ record newName newNode
  | otherwise = error "complement: not sets"

subsetFnDef :: Node
subsetFnDef = Relation universal universal []

subset :: String -> Record -> Context
subset name (nameA, a@(Set def tags)) = do
    (nodes, idGen) <- get
    let newId = idGen
    let newNode = Set def tags
    let newStatement = newNode `isSubsetOf` a
    addNewNode name newNode
    addNewStatement newStatement
subset _ _ = return ()

{-| returns a statement actually -}
isSubsetOf :: Node -> Node -> Node
isSubsetOf a b =
  let partialApp = App subsetFnDef a in
  let fullApp = App (Class partialApp universal []) b in
  Statement fullApp []

isSubsetOf' :: Node -> Node -> State (Graph, Int) Bool
isSubsetOf' a b = do
  graph <- getNodes
  case findFirst (== a `isSubsetOf` b) graph of
    Just _ -> return True
    Nothing -> return False

universal :: Node
universal = Set Universal ["Universal"]

empty :: Node
empty = Set Empty ["Empty"]

setOfDirectProducts :: Node
setOfDirectProducts = Set (Type $ Tuple [universal, universal] []) []

powersetFnDef :: Node
powersetFnDef = 
  let allSubsets = Class (App subsetFnDef universal) universal [] in
  Unary Definition universal (Set (Type allSubsets) []) []

powerset :: Record -> Context
powerset (name, set)
  | isSet set = do
    (nodes, _) <- get
    application <-  set `applyR'ed` powersetFnDef
    let newNode = Set (Type application) []
    let newName = "P(" ++ name ++ ")"
    addNewNode newName newNode
    addNewStatement (set `isSubsetOf` newNode)
  | otherwise = return ()

applyR'ed :: Node -> Node -> PContext Node
applyR'ed binop@(Binary l r o ts) v = do
  nodes <- getNodes
  subset <- isSubsetOf' v l  
  if subset then
    return $ Unary (App binop v) r o ts
  else error "apply binary"
applyR'ed un@(Unary u v o ts) v' = do
  subset <- isSubsetOf' v' v
  if subset then return o
  else error "apply unary"
applyR'ed rel@(Relation l r ts) v = do
  subset <- isSubsetOf' v l
  if subset then 
    return $ Class (App rel v) r ts
  else error "apply relation"
applyR'ed fixed@(Class u r ts) v = do
  subset <- isSubsetOf' r v 
  if subset then
    return $ Statement (App fixed v) ts
  else error "apply fixed"
applyR'ed _ _ = error "apply no match" 


