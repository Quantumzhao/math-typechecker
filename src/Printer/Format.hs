module Printer.Format where

import Node hiding (tags)
import Printer.FormatDef
import ContextState
import Set

formatNode :: Node -> Graph -> Expr
formatNode (Mapping domain range tags i) nodes = 
  let dWhere = getSimpleWhereExpr domain nodes in
  let rWhere = getSimpleWhereExpr range nodes in
  MappingExpr {
    name = getName i,
    tags = tags,
    left = formatSetBody domain,
    right = formatSetBody range,
    wheres = mergeWheres [dWhere, rWhere]
  }
formatNode s@(Collection def tags i) nodes = 
  SetExpr {
    name = getName i,
    tags = tags,
    body = formatSetBody s,
    wheres = getSimpleWhereExpr s nodes
  } 
formatNode t@(DirectProduct (left, right) i) nodes = 
  let lWhere = getSimpleWhereExpr (findParent left nodes) nodes in
  let rWhere = getSimpleWhereExpr (findParent right nodes) nodes in
  TupleExpr {
    name = getName i,
    first = nameOf $ key left,
    second = nameOf $ key right,
    wheres = mergeWheres [lWhere, rWhere]
  }
formatNode o@(Object i) nodes = 
  let parent = findParent o nodes in
  let whereExpr = getSimpleWhereExpr parent nodes in
  ObjectExpr {
    name = nameOf $ key o,
    set = nameOf $ key parent,
    wheres = whereExpr
  }
formatNode (Relation domain codomain tags i) nodes = 
  let leftParent = findParent domain nodes in
  let rightParent = findParent codomain nodes in
  let leftWhere = toWhereExpr domain leftParent nodes in
  let rightWhere = toWhereExpr codomain rightParent nodes in
  RelExpr {
    left = formatSetBody domain,
    right = formatSetBody codomain,
    by = nameOf i,
    tags = tags,
    forallLeft = ForAll $ nameOf $ key leftParent,
    forallRight = ForAll $ nameOf $ key rightParent,
    wheres = mergeWheres [leftWhere, rightWhere]
  }
  where
    toWhereExpr node parent nodes = 
      if parent == anyObject then BlankWhere else Clause [formatNode parent nodes]

getName :: Identifier -> String
getName (Unique name id) = name
getName Anonymous = error "Format.getName: how did we get there?"

getSimpleWhereExpr :: Node -> Graph -> WhereExpr
getSimpleWhereExpr parent nodes = 
  if parent == anyObject then BlankWhere else Clause [formatNode parent nodes]

formatSetBody :: Node -> SetBodyExpr 
formatSetBody (Collection (Multiple es) tags _) = SetContaining (fmap (nameOf . key) es)
formatSetBody (Collection (FormOf someType) tags _) = SetTypeOfExpr (nameOf $ key someType)
formatSetBody _ = undefined 

mergeWheres :: [WhereExpr] -> WhereExpr
mergeWheres = undefined 

-- "parent" is referring to the set which is the greatest lower bound of a lattice
-- formed by the subset relation and all sets that contains this element
findParent :: Node -> Graph -> Node
findParent node nodes = undefined
  where findParent' node nodes candidate = undefined 

