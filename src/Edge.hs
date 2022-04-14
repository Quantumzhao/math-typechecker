module Edge where
import Mapping (Mapping)
import Node (Node)

data Edge
  = Rel Relation
  | Map Mapping
  | BinOp Node Node BinaryOperationType

data BinaryOperationType
  = Intersect
  | Union
  | Cross
  | RelCompl

