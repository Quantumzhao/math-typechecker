module Statement where

import Node
import ContextState
import Data.Map

existStmt :: Node -> Node -> Node -> Map String Node -> Bool
existStmt a b rel nodes = 
  undefined 