module Interpreter.Evaluate where
import ContextState
import Control.Monad.State.Lazy
import Printer.FormatDef hiding (Clause, tags)
import Interpreter.AST hiding (tags)
import Node hiding (Definition)
import Printer.Format
import Set

data ReturnType
    = Res Node
    | Err String
    | Halt

evaluate :: Command -> PContext ReturnType
evaluate com =
  case com of
    Definition def -> do
      nodes <- getNodes
      res <- evalDefinition def
      return $ Res res
    Info symbol -> evalInfo symbol
    Exit -> return Halt
    AnonymousExpr expr -> evalAnonymousExpr expr

evalWithEnv ::  GraphI -> Command -> (ReturnType, GraphI)
evalWithEnv env com = runState (evaluate com) env

evalInfo symbol = undefined

evalAnonymousExpr expr = undefined

evalDefinition :: DefEntry -> PContext Node
evalDefinition (DefEntry name body closure) = do
  evalClosure closure
  node' <- evalMathDef name body
  addNewNode node'

evalClass :: String -> Class -> PContext Node
evalClass name classDef = undefined

-- convert the definition to a node
-- then return it
evalMathDef :: String -> MathDef -> PContext Node
evalMathDef name (FromClassAST c) = undefined
evalMathDef name (FromMappingAST (MappingDef (Symbol domain) (Symbol range) tags)) = do
  id <- getNewId
  d' <- findByNameM' domain
  r' <- findByNameM' range
  let res = Mapping {
    domain = d',
    range = r',
    tags = tags,
    key = Unique name id
  }
  return res
evalMathDef name (FromRelationAST (RelDef (Symbol from) (Symbol to) tags)) = do
  id <- getNewId
  from' <- findByNameM' from
  to' <- findByNameM' to
  let res = Relation {
    domain = from',
    codomain = to',
    tags = tags,
    key = Unique name id
  }
  return res
evalMathDef name (FromObjectAST (ObjectDef (Symbol set))) = do
  id <- getNewId
  set' <- findByNameM' set
  let res = Object set' (Unique name id)
  return res
evalMathDef name (FromTupleAST (TupleDef (Symbol left) (Symbol right) tags)) = do
  id <- getNewId
  left' <- findByNameM' left
  right' <- findByNameM' right
  let res = DirectProduct (left', right') (Unique name id)
  return res
evalMathDef name (FromSymbol s) = do
  n <- findByNameM' name
  Alias n . Unique name <$> getNewId

-- add closure to the environment
-- and then return the closure
evalClosure :: Closure -> PContext [Node]
evalClosure (d : ds) = do
  n <- evalDefinition d
  addNewNode n
  ns <- evalClosure ds
  return (n : ns)
evalClosure [] = return []

findByNameM' :: String -> PContext Node
findByNameM' name = do
  node <- findByNameM name
  case node of
    Nothing -> error "findByNameM': node not defined"
    Just n -> return n

findInClosure :: String -> Closure -> Maybe DefEntry
findInClosure name (x@(DefEntry sym _ _) : xs)
  | sym == name = Just x
  | otherwise = findInClosure name xs
findInClosure name [] = Nothing
