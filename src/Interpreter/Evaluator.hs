module Interpreter.Evaluator where
import ContextState
  ( Context,
    PContext,
    Environment,
    addNewNode,
    findByNameM',
    getNewId )
import Control.Monad.State.Lazy ( StateT(runStateT) )
import Interpreter.AST
  ( MathExp(..),
    Closure,
    Claim(Claim),
    Symbol(Symbol),
    Tuple(TupleDef),
    Object(ObjectDef),
    Relation(RelDef),
    Mapping(MappingDef),
    Class(ClassDef),
    MathDef(..),
    DefEntry(DefEntry),
    Command(..) )
import Node ( Identifier(..), Node(..) )
import Relation ( isIn )
import Control.Monad.Except ( runExcept, MonadError(throwError) )
import Mapping ( (<.>), applyArg )

data ReturnType
  = Res Node
  | Halt
  deriving (Show, Eq)

evaluate :: Command -> PContext ReturnType
evaluate com =
  case com of
    Definition def -> do
      res <- evalDefinition def False
      addNewNode res
      return $ Res res
    Exit -> return Halt
    AnonymousExpr expr -> do
      res <- evalExpr expr
      return $ Res res
    ClaimOf c -> do
      res <- evalClaim c
      addNewNode res
      return $ Res res

{-| also puts the result into the graph -}
evaluateMany :: [Command] -> PContext ReturnType
evaluateMany [] = return Halt
evaluateMany (x : xs) = do
  res <- evaluate x
  evaluateMany xs

evalWithEnv ::  Environment -> Command -> Either String (ReturnType, Environment)
evalWithEnv env com = runExcept $ runStateT (evaluate com) env

updateState :: Context -> Environment -> Either String Environment
updateState stateTrans env = 
  case runExcept $ runStateT stateTrans env of
    Left msg -> Left msg
    Right (_, env') -> Right env'

evalExpr :: MathExp -> PContext Node
evalExpr (Apply1 (Symbol name) exp1) = do
  arg <- evalExpr exp1
  f' <- findByNameM' name
  f <- case f' of
        Mapping {} -> return f'
        _ -> throwError "evalExpr Apply1: f is not a mapping"
  applyArg f' arg
evalExpr (Apply2 (Symbol name) exp1 exp2) = do
  arg1 <- evalExpr exp1
  arg2 <- evalExpr exp2
  f' <- findByNameM' name
  f <- case f' of
        Mapping {} -> return f'
        _ -> throwError "evalExpr Aply1: f is not a function"
  tup <- arg1 <.> arg2
  applyArg f tup
-- it makes no sense to evaluate a relation, since if a relation can only be evaluated if it exists, 
-- and the existence implies that the result is always true
evalExpr (Relate (Symbol name) exp1 exp2) = throwError "cannot evaluate a relation"
evalExpr (Tuple exp1 exp2) = do
  left <- evalExpr exp1
  right <- evalExpr exp2
  id <- getNewId
  let res = DirectProduct (left, right) (Exist (tupleNotation (nameOf $ key left) (nameOf $ key right)) id)
  return res
  where tupleNotation l r = "(" ++ l ++ ", " ++ r ++ ")"
evalExpr (Variable (Symbol name)) = findByNameM' name

evalDefinition :: DefEntry -> Bool -> PContext Node
evalDefinition (DefEntry name body closure) isTemplate = do
  evalClosure closure
  id <- getNewId
  let i = if isTemplate then ForAll else Exist name id
  evalMathDef body i

-- convert the definition to a node
-- then return it
evalMathDef :: MathDef -> Identifier -> PContext Node
evalMathDef (FromClassAST (ClassDef tags)) key = do
  let res = Class tags key
  return res
evalMathDef (FromMappingAST (MappingDef domain range tags)) key = do
  d' <- evalExpr domain
  r' <- evalExpr range
  let res = Mapping {
    domain = d',
    range = r',
    tags = tags,
    key = key
  }
  return res
evalMathDef (FromRelationAST (RelDef from to tags)) key = do
  from' <- evalExpr from
  to' <- evalExpr to
  let res = Relation {
    domain = from',
    codomain = to',
    tags = tags,
    key = key
  }
  return res
evalMathDef (FromObjectAST (ObjectDef set)) key = do
  set' <- evalExpr set
  let res = Object key
  claim <- res `isIn` set'
  addNewNode claim
  return res
evalMathDef (FromTupleAST (TupleDef left right)) key = do
  left' <- evalExpr left
  right' <- evalExpr right
  let res = DirectProduct (left', right') key
  return res
evalMathDef (FromExpr s) key = do
  n <- evalExpr s
  return $ Alias n key

evalClaim :: Claim -> PContext Node
evalClaim (Claim from to rel) = do
  f <- evalExpr from
  t <- evalExpr to
  r <- evalExpr rel
  id <- getNewId
  return $ ClaimOfRel {
    from = f,
    to = t,
    Node.relation = r,
    key = Exist id id
  }

-- add closure to the environment
-- and then return the closure
evalClosure :: Closure -> PContext [Node]
evalClosure ((isTemplate, d) : ds) = do
  n <- evalDefinition d isTemplate
  addNewNode n
  ns <- evalClosure ds
  return (n : ns)
evalClosure [] = return []
