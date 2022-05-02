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
      res <- evalDefinition def True
      return $ Res res
    Info symbol -> evalInfo symbol
    Exit -> return Halt
    AnonymousExpr lit expr -> do
      res <- evalAnonymousExpr expr
      return $ Res res

evalWithEnv ::  GraphI -> Command -> (ReturnType, GraphI)
evalWithEnv env com = runState (evaluate com) env

evalInfo symbol = undefined

evalAnonymousExpr :: MathExp -> PContext Node
evalAnonymousExpr (Apply1 (Symbol name) exp1) = undefined
evalAnonymousExpr (Apply2 (Symbol name) exp1 exp2) = undefined
evalAnonymousExpr (Relate (Symbol name) exp1 exp2) = undefined
evalAnonymousExpr (Tuple exp1 exp2) = do
  let left = evalAnonymousExpr exp1
  let right = evalAnonymousExpr exp2
  undefined
evalAnonymousExpr (Variable (Symbol name)) = findByNameM' name

evalDefinition :: DefEntry -> Bool -> PContext Node
evalDefinition (DefEntry name body closure) isTemplate = do
  evalClosure closure
  id <- getNewId
  let i = if isTemplate then Arbitrary else Unique name id
  node' <- evalMathDef name body i
  addNewNode node'

-- convert the definition to a node
-- then return it
evalMathDef :: String -> MathDef -> Identifier -> PContext Node
evalMathDef name (FromClassAST (ClassDef tags)) key = do
  let res = Class tags key
  return res
evalMathDef name (FromMappingAST (MappingDef (Symbol domain) (Symbol range) tags)) key = do
  d' <- findByNameM' domain
  r' <- findByNameM' range
  let res = Mapping {
    domain = d',
    range = r',
    tags = tags,
    key = key
  }
  return res
evalMathDef name (FromRelationAST (RelDef (Symbol from) (Symbol to) tags)) key = do
  from' <- findByNameM' from
  to' <- findByNameM' to
  let res = Relation {
    domain = from',
    codomain = to',
    tags = tags,
    key = key
  }
  return res
evalMathDef name (FromObjectAST (ObjectDef (Symbol set))) key = do
  set' <- findByNameM' set
  let res = Object set' key
  return res
evalMathDef name (FromTupleAST (TupleDef (Symbol left) (Symbol right) tags)) key = do
  left' <- findByNameM' left
  right' <- findByNameM' right
  let res = DirectProduct (left', right') key
  return res
evalMathDef name (FromSymbol s) key = do
  n <- findByNameM' name
  return $ Alias n key

-- add closure to the environment
-- and then return the closure
evalClosure :: Closure -> PContext [Node]
evalClosure ((isTemplate, d) : ds) = do
  n <- evalDefinition d isTemplate
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

-- findInClosure :: String -> Closure -> Maybe DefEntry
-- findInClosure name (x@(DefEntry sym _ _) : xs)
--   | sym == name = Just x
--   | otherwise = findInClosure name xs
-- findInClosure name [] = Nothing
