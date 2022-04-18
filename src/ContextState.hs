{-# LANGUAGE FlexibleContexts #-}
module ContextState where
import Common
import Data.Map
import Control.Monad.State.Lazy (State, get, put)
import Control.Monad.Except
import Control.Monad.State
import Node

type Record = (String, Node)
type Graph = Map String Node
type GraphI = (Graph, Int)
type PContext = State GraphI
type Context = PContext ()

{-| accepts any node except statements. 
    In which case, use `addNewStatement` instead -}
addNewNode :: String -> Node -> PContext Record
addNewNode name node = do
  (nodes, idGen) <- get
  name' <- nextName name nodes
  let nodes' = insert name' node nodes
  put (nodes', idGen)
  return $ record name' node
  where nextName name map
          | not $ member name map = return name
          | otherwise = do 
              (_, id) <- get
              move2NextId
              nextName (name ++ show id) map

{-| only accepts statements -}
addNewStatement :: Node -> Context
addNewStatement stmt = do
  (nodes, idGen) <- get
  let name = "statement" ++ show idGen
  let idGen' = idGen + 1
  let nodes' = insert name stmt nodes
  put (nodes', idGen')

move2NextId :: Context
move2NextId = modify (\(ns, id) -> (ns, id + 1))

findFirst :: (Node -> Bool) -> Graph -> Maybe Node
findFirst f map =
  findFirst' f (elems map)
  where 
    findFirst' f (x : xs) = 
      if f x then Just x
      else findFirst' f xs
    findFirst' f [] = Nothing

getNodes :: State (Graph, Int) Graph
getNodes = do
  (nodes, _) <- get
  return nodes

record :: String -> Node -> Record
record = (,)

-- addNewEdge :: Edge -> GraphState
-- addNewEdge edge = do
--   (Context ns edges c) <- get
--   let nextC = c + 1
--   let newMap = Data.Map.insert nextC edge edges
--   put (Context ns newMap nextC)
--   return ()

