module Statement where

import Node
import ContextState
import Data.Map

{-| `rel` must be a statement -}
existStmt :: Node -> Graph -> Bool
existStmt rel nodes = 
  case findFirst (== rel) nodes of
    Just _ -> True
    Nothing -> False