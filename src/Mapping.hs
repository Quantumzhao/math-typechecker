module Mapping where

import Node ( Identifier(..), Node(..) )
import ContextState ( PContext, addNewNode, getNewId )
import Relation ( isIn, isInB, isSubsetOfB )
import Control.Monad.Except ( MonadError(throwError) )
import Util ( toLower )
import Set (isSameAs)

-- short for cross
(<.>) :: Node -> Node -> PContext Node
(<.>) n1 n2 = do
  newId <- getNewId
  let res = DirectProduct (n1, n2) (Exist "cross" newId)
  return res

-- apply argument to functions
-- the domain can be:
-- 1. cartesian product of sets
-- 2. ordered pair of elements
-- 3. set
-- 4. element
applyArg :: Node -> Node -> PContext Node
applyArg (Mapping domain range tags i) arg = do
  -- check if the argument is a subset of domain
  isSubset <- arg `isSubsetOfB` domain
  -- check if the argument is in domain
  isInFlag <- arg `isInB` domain
  if isSubset || (arg `isSameAs` domain) then return range
  -- if it is an element, then create the corresponding element in the image
  else if isInFlag then do
    id <- getNewId
    let o = Object (Exist (toLower (nameOf $ key range)) id)
    claim <- o `isIn` range
    addNewNode claim
    return o
  else throwError "applyArg: arg is not related to domain"
applyArg _ _ = throwError "applyArg: not a mapping"
