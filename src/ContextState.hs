module ContextState where
import Control.Monad.State
import Control.Monad.Except
import Node

type Nodes = [Node]
type Environment = (Nodes, Int)
type PContext = StateT Environment (Except String)
type Context = PContext ()

{-| accepts any node except statements -}
addNewNode :: Node -> PContext ()
addNewNode node = do
  (nodes, idGen) <- get
  let nodes' = node : nodes
  put (nodes', idGen)

{-| only accepts statements -}
-- addNewStatement :: Node -> Context
-- addNewStatement stmt = do
--   (nodes, idGen) <- get
--   let name = "statement" ++ show idGen
--   let idGen' = idGen + 1
--   let nodes' = stmt : nodes
--   put (nodes', idGen')

-- addNewStatementM :: PContext Node -> Context
-- addNewStatementM stmt = do
--   stmt' <- stmt
--   addNewStatement stmt'

move2NextId :: Context
move2NextId = modify (\(ns, id) -> (ns, id + 1))

{-| find the first occurence of a node satisfying the condition -}
findFirst :: (Node -> Bool) -> Nodes -> Maybe Node
findFirst f (x : xs) =
  if f x then Just x
  else findFirst f xs
findFirst f [] = Nothing

{-| a monadic version of `findFirst` -}
findFirstM :: (Node -> PContext Bool) -> PContext (Maybe Node)
findFirstM f = do
  ns <- getNodes
  res <- filterM f ns
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

findByName :: String -> Nodes -> Maybe Node
findByName name = findFirst (\ n -> nameOf (key n) == name)

{-| do it in a monadic context -}
findByNameM :: String -> PContext (Maybe Node)
findByNameM name = findFirst (\ n -> nameOf (key n) == name) <$> getNodes

{-| the caller must be certain that such a thing exist -}
findByNameM' :: String -> PContext Node
findByNameM' name = do
  node <- findByNameM name
  case node of
    Nothing -> throwError $ "findByNameM': " ++ name ++ " not defined"
    Just n -> return n

getNodes :: PContext Nodes
getNodes = do
  (nodes, _) <- get
  return nodes

getRelations :: PContext [Node]
getRelations = filter match <$> getNodes where
  match Relation {} = True
  match _ = False

getNewId :: PContext String
getNewId = do
  (nodes, id) <- get
  put (nodes, id + 1)
  return $ show id

{-| the caller must be certain that such a thing exist -}
getNodeByName :: String -> PContext Node
getNodeByName name = do
  res <- findByNameM name
  case res of
    Nothing -> throwError $ name ++ " not found"
    Just sth -> return sth
