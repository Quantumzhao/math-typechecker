{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module ContextState where
import Control.Monad.State
import Control.Monad.Except
import Node

type Nodes = [Node]
type Environment = (Nodes, Int)
type PContext = StateT Environment (Except String)
type Context = PContext ()

{-| accepts any node except statements. 
    In which case, use `addNewStatement` instead -}
addNewNode :: Node -> PContext Node
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

addNewStatementM :: PContext Node -> Context
addNewStatementM stmt = do
  stmt' <- stmt
  addNewStatement stmt'

move2NextId :: Context
move2NextId = modify (\(ns, id) -> (ns, id + 1))

findFirst :: (Node -> Bool) -> Nodes -> Maybe Node
findFirst f (x : xs) =
  if f x then Just x
  else findFirst f xs
findFirst f [] = Nothing

findByName :: String -> Nodes -> Maybe Node
findByName name = findFirst (\ n -> nameOf (key n) == name)

findByNameM :: String -> PContext (Maybe Node)
findByNameM name = findFirst (\ n -> nameOf (key n) == name) <$> getNodes

getNodes :: PContext Nodes
getNodes = do
  (nodes, _) <- get
  return nodes

getRelations :: PContext [Node]
getRelations = filter match <$> getNodes where
  match Relation {} = True
  match _ = False

-- getNodes' :: Test [Node]
-- getNodes' = do
--   (nodes, _) <- get
--   return nodes


getNewId :: PContext String
getNewId = do
  (nodes, id) <- get
  put (nodes, id + 1)
  return $ show id
