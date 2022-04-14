module ContextState where
import Node
import Common

data Context = Context {
  nodes :: [Node],
  relations :: [Relation Node Node]
}


