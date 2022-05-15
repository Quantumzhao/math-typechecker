module Printer.Format where

import Node hiding (tags)
import Printer.FormatDef hiding (ForAll, Exist)
import ContextState
import Set
import Control.Monad.Except
import Relation
import Data.Maybe

formatNode :: Node -> PContext Expr
formatNode (Mapping domain range tags i) = do
  nodes <- getNodes
  let dWhere = toWhereExpr domain nodes
  let rWhere = toWhereExpr range nodes
  name <- getName i
  return MappingExpr {
    name = name,
    tags = tags,
    left = nameOf $ key domain,
    right = nameOf $ key range,
    wheres = mergeWheres [dWhere, rWhere]
  }
formatNode s@(Class tags i) = do
  nodes <- getNodes
  name <- getName i
  return SetExpr {
    name = name,
    tags = tags,
    wheres = toWhereExpr s nodes
  }
formatNode t@(DirectProduct (left, right) i) = do
  nodes <- getNodes
  let lWhere = toWhereExpr left nodes
  let rWhere = toWhereExpr right nodes
  name <- getName i
  return TupleExpr {
    name = name,
    first = nameOf $ key left,
    second = nameOf $ key right,
    wheres = mergeWheres [lWhere, rWhere]
  }
formatNode o@(Object i) = do
  res <- findFirstM criteria
  u <- getUniverse
  let t = fromMaybe u res
  --let whereExpr = toWhereExpr t nodes in
  return ObjectExpr {
    name = nameOf $ key o,
    set = nameOf $ key t,
    wheres = BlankWhere
  }
  where criteria c@Class {} = do
          isInRel <- get'isIn'relation
          existClaim (o `relatesTo` c `by` isInRel)
        criteria _ = return False
formatNode (Relation domain codomain tags i) = do
  nodes <- getNodes
  let leftWhere = toWhereExpr domain nodes
  let rightWhere = toWhereExpr codomain nodes
  return RelExpr {
    left = nameOf $ key domain,
    right = nameOf $ key codomain,
    name = nameOf i,
    tags = tags,
    wheres = mergeWheres [leftWhere, rightWhere]
  }
formatNode (Alias n i) = formatNode (trackAlias n)
formatNode ClaimOfRel {} = return ClaimExpr

getName :: Identifier -> PContext String
getName (Exist name id) = return name
getName ForAll = throwError "Format.getName: how did we get there?"

-- for demo only
mergeWheres :: [WhereExpr] -> WhereExpr
mergeWheres [] = BlankWhere
mergeWheres (x : xs) = x

toWhereExpr :: Node -> Nodes -> WhereExpr
toWhereExpr node graph = BlankWhere
