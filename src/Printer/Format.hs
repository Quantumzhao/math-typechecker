module Printer.Format where

import Node hiding (tags)
import Printer.FormatDef hiding (ForAll, Exist)
import ContextState
import Set

formatNode :: Node -> Graph -> Expr
formatNode (Mapping domain range tags i) nodes = 
  let dWhere = toWhereExpr domain nodes in
  let rWhere = toWhereExpr range nodes in
  MappingExpr {
    name = getName i,
    tags = tags,
    left = nameOf $ key domain,
    right = nameOf $ key range,
    wheres = mergeWheres [dWhere, rWhere]
  }
formatNode s@(Class tags i) nodes = 
  SetExpr {
    name = getName i,
    tags = tags,
    wheres = toWhereExpr s nodes
  } 
formatNode t@(DirectProduct (left, right) i) nodes = 
  let lWhere = toWhereExpr left nodes in
  let rWhere = toWhereExpr right nodes in
  TupleExpr {
    name = getName i,
    first = nameOf $ key left,
    second = nameOf $ key right,
    wheres = mergeWheres [lWhere, rWhere]
  }
formatNode o@(Object i) nodes = 
  let t = undefined in
  let whereExpr = toWhereExpr t nodes in
  ObjectExpr {
    name = nameOf $ key o,
    set = nameOf $ key t,
    wheres = whereExpr
  }
formatNode (Relation domain codomain tags i) nodes = 
  let leftWhere = toWhereExpr domain nodes in
  let rightWhere = toWhereExpr codomain nodes in
  RelExpr {
    left = nameOf $ key domain,
    right = nameOf $ key codomain,
    name = nameOf i,
    tags = tags,
    wheres = mergeWheres [leftWhere, rightWhere]
  }
formatNode (Alias n i) nodes = undefined

getName :: Identifier -> String
getName (Exist name id) = name
getName ForAll = error "Format.getName: how did we get there?"

-- for demo only
mergeWheres :: [WhereExpr] -> WhereExpr
mergeWheres [] = BlankWhere 
mergeWheres (x : xs) = x 

toWhereExpr :: Node -> Graph -> WhereExpr
toWhereExpr node graph = BlankWhere 
