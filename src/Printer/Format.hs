module Printer.Format where

import Node hiding (tags)
import Printer.FormatDef
import ContextState (Graph)
import Set

formatNode :: Node -> Graph -> Expr
formatNode (Mapping domain range tags name _) nodes = 
  let dWhere = getSimpleWhereExpr domain nodes in
  let rWhere = getSimpleWhereExpr range nodes in
  MappingExpr {
    name = name,
    tags = tags,
    left = formatSetBody domain,
    right = formatSetBody range,
    wheres = mergeWheres [dWhere, rWhere]
  }
formatNode s@(Set def tags name _) nodes = 
  SetExpr {
    name = name,
    tags = tags,
    body = formatSetBody s,
    wheres = getSimpleWhereExpr s nodes
  } 
formatNode t@(Tuple (left, right) name _) nodes = 
  let lWhere = getSimpleWhereExpr (findParent left nodes) nodes in
  let rWhere = getSimpleWhereExpr (findParent right nodes) nodes in
  TupleExpr {
    name = name,
    first = nameOf left,
    second = nameOf right,
    wheres = mergeWheres [lWhere, rWhere]
  }
formatNode o@(Object name _) nodes = 
  let parent = findParent o nodes in
  let whereExpr = getSimpleWhereExpr parent nodes in
  ObjectExpr {
    name = name,
    set = nameOf parent,
    wheres = whereExpr
  }
formatNode (Relation domain codomain tags name _) nodes = 
  let leftParent = findParent domain nodes in
  let rightParent = findParent codomain nodes in
  let leftWhere = toWhereExpr domain leftParent nodes in
  let rightWhere = toWhereExpr codomain rightParent nodes in
  RelExpr {
    left = formatSetBody domain,
    right = formatSetBody codomain,
    by = name,
    tags = tags,
    forallLeft = ForAll $ nameOf leftParent,
    forallRight = ForAll $ nameOf rightParent,
    wheres = mergeWheres [leftWhere, rightWhere]
  }
  where
    toWhereExpr node parent nodes = 
      if parent == universal then BlankWhere else Clause [formatNode parent nodes]

getSimpleWhereExpr :: Node -> Graph -> WhereExpr
getSimpleWhereExpr parent nodes = 
  if parent == universal then BlankWhere else Clause [formatNode parent nodes]

formatSetBody :: Node -> SetBodyExpr 
formatSetBody (Set (Collection es) tags name _) = SetContaining (fmap nameOf es)
formatSetBody (Set (FormOf someType) tags name _) = SetTypeOfExpr (nameOf someType)
formatSetBody _ = undefined 

mergeWheres :: [WhereExpr] -> WhereExpr
mergeWheres = undefined 

-- "parent" is referring to the set which is the greatest lower bound of a lattice
-- formed by the subset relation and all sets that contains this element
findParent :: Node -> Graph -> Node
findParent node nodes = undefined
  where findParent' node nodes candidate = undefined 

