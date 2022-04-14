module ContextState where
import Common
import Data.Map
import Control.Monad.State.Lazy (State, get, put)
import Node (Node)

data Context = Context {
  nodes :: Map Int Node,
  counter :: Int
}

type GraphState = State Context ()

addNewNode :: Node -> GraphState
addNewNode node = do
  (Context nodes c) <- get
  let nextC = c + 1
  let nodes' = insert nextC node nodes
  put (Context nodes' nextC)
  return ()

-- addNewEdge :: Edge -> GraphState
-- addNewEdge edge = do
--   (Context ns edges c) <- get
--   let nextC = c + 1
--   let newMap = Data.Map.insert nextC edge edges
--   put (Context ns newMap nextC)
--   return ()

