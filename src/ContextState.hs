module ContextState where
import Control.Monad.State ( filterM, modify, MonadState(put, get), StateT )
import Control.Monad.Except ( MonadError(throwError), Except )
import Node ( Identifier(nameOf), Node(Relation, key) )

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
