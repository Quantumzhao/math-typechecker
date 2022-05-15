module Printer.Format where

import Node hiding (tags)
import Printer.FormatDef hiding (ForAll, Exist)
import ContextState
import Set
import Control.Monad.Except

formatNode :: Node -> Nodes -> Except String Expr
formatNode (Mapping domain range tags i) nodes = do
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
formatNode s@(Class tags i) nodes = do
  name <- getName i
  return SetExpr {
    name = name,
    tags = tags,
    wheres = toWhereExpr s nodes
  } 
formatNode t@(DirectProduct (left, right) i) nodes = do
  let lWhere = toWhereExpr left nodes
  let rWhere = toWhereExpr right nodes
  name <- getName i
  return TupleExpr {
    name = name,
    first = nameOf $ key left,
    second = nameOf $ key right,
    wheres = mergeWheres [lWhere, rWhere]
  }
formatNode o@(Object i) nodes = 
  let t = undefined in
  let whereExpr = toWhereExpr t nodes in
  return ObjectExpr {
    name = nameOf $ key o,
    set = nameOf $ key t,
    wheres = whereExpr
  }
formatNode (Relation domain codomain tags i) nodes =
  let leftWhere = toWhereExpr domain nodes in
  let rightWhere = toWhereExpr codomain nodes in
  return RelExpr {
    left = nameOf $ key domain,
    right = nameOf $ key codomain,
    name = nameOf i,
    tags = tags,
    wheres = mergeWheres [leftWhere, rightWhere]
  }
formatNode (Alias n i) nodes = formatNode (trackAlias n) nodes

getName :: Identifier -> Except String String
getName (Exist name id) = return name
getName ForAll = throwError "Format.getName: how did we get there?"

-- for demo only
mergeWheres :: [WhereExpr] -> WhereExpr
mergeWheres [] = BlankWhere 
mergeWheres (x : xs) = x 

toWhereExpr :: Node -> Nodes -> WhereExpr
toWhereExpr node graph = BlankWhere 
