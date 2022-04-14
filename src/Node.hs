module Node where

data Node
  = Element
  | Set {
    name :: String,
    tags :: [String]
  }
  | Group {
    name :: String, 
    tags :: [String]
  }
  | Map {
    domain :: Node,
    range :: Node,
    tags :: [String]
  }
  | Relation {
    e1 :: Node,
    e2 :: Node,
    _type :: StructuralRelation,
    tags :: [String]
  }
  | BinOp Node Node BinaryOperationType

data BinaryOperationType
  = Intersect
  | Union
  | Cross
  | RelCompl

data StructuralRelation
  = Subset

areSets :: [Node] -> Bool
areSets = all matchSet
  where matchSet (Set _ _) = True
        matchSet _ = False
