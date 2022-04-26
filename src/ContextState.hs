{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module ContextState where
import Control.Monad.State.Lazy (State, get, put)
import Control.Monad.Except
import Control.Monad.State
import Node

type Graph = [Node]
type GraphI = (Graph, Int)
type StateAnd = State GraphI
type Context = StateAnd ()

type Test m a = (MonadState GraphI m, MonadError (String, String) m) => m a

{-| accepts any node except statements. 
    In which case, use `addNewStatement` instead -}
addNewNode :: Node -> StateAnd Node
addNewNode node = do
  (nodes, idGen) <- get
  let nodes' = node : nodes
  put (nodes', idGen)
  return node

{-| only accepts statements -}
addNewStatement :: Node -> Context
addNewStatement stmt = do
  (nodes, idGen) <- get
  let name = "statement" ++ show idGen
  let idGen' = idGen + 1
  let nodes' = stmt : nodes
  put (nodes', idGen')

addNewStatementM :: StateAnd Node -> Context
addNewStatementM stmt = do
  stmt' <- stmt
  addNewStatement stmt'

move2NextId :: Context
move2NextId = modify (\(ns, id) -> (ns, id + 1))

findFirst :: (Node -> Bool) -> Graph -> Maybe Node
findFirst f (x : xs) = 
  if f x then Just x
  else findFirst f xs
findFirst f [] = Nothing

getNodes :: StateAnd [Node]
getNodes = do
  (nodes, _) <- get
  return nodes

-- getNodes' :: Test [Node]
-- getNodes' = do
--   (nodes, _) <- get
--   return nodes


getNewId :: StateAnd String
getNewId = do
  (nodes, id) <- get
  put (nodes, id + 1)
  return $ show id

-- addNewEdge :: Edge -> GraphState
-- addNewEdge edge = do
--   (Context ns edges c) <- get
--   let nextC = c + 1
--   let newMap = Data.Map.insert nextC edge edges
--   put (Context ns newMap nextC)
--   return ()

anonymous :: String
anonymous = ""