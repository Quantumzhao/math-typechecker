{-# LANGUAGE FlexibleContexts #-}
module ContextState where
import Common
import Data.Map
import Control.Monad.State.Lazy (State, get, put)
import Node (Node)
import Control.Monad.Except
import Control.Monad.State

type Record = (String, Node)
type Context = State (Map String Node, Int) ()

{-| accepts any node except statements. 
    In which case, use `addNewStatement` instead -}
addNewNode :: String -> Node -> Context
addNewNode name node = do
  (nodes, idGen) <- get
  let name' = nextName name nodes
  let nodes' = insert name' node nodes
  put (nodes', idGen)
  where nextName name map
          | not $ member name map = name
          | otherwise = nextName (name ++ "\'") map

{-| only accepts statements -}
addNewStatement :: Node -> Context
addNewStatement stmt = do
  (nodes, idGen) <- get
  let name = "statement" ++ show idGen
  let idGen' = idGen + 1
  let nodes' = insert name stmt nodes
  put (nodes', idGen')

-- addNewEdge :: Edge -> GraphState
-- addNewEdge edge = do
--   (Context ns edges c) <- get
--   let nextC = c + 1
--   let newMap = Data.Map.insert nextC edge edges
--   put (Context ns newMap nextC)
--   return ()

