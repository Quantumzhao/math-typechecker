module ContextState where
import Common
import Data.Map
import Control.Monad.State.Lazy (State, get, put)
import Node (Node)
import Edge (Edge)

data Context = Context {
  nodes :: [Node],
  edges :: Map Int Edge,
  counter :: Int
}

type GraphState = State Context ()

addNewNode :: (Int -> Node) -> GraphState
addNewNode nodeP = do
  (Context nodes es c) <- get
  put (Context (nodeP (c + 1) : nodes) es (c + 1))
  return ()

addNewEdge :: Edge -> GraphState
addNewEdge edge = do
  (Context ns edges c) <- get
  let nextC = c + 1
  let newMap = Data.Map.insert nextC edge edges
  put (Context ns newMap nextC)
  return ()

