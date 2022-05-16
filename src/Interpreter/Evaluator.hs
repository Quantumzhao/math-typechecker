module Interpreter.Evaluator where
import ContextState
import Control.Monad
import Control.Monad.State.Lazy hiding (void)
import Printer.FormatDef hiding (Clause, tags, Exist, ForAll)
import Interpreter.AST hiding (tags)
import Node hiding (Definition)
import Printer.Format
import Relation
import Set
import Interpreter.Parser
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Except (runExcept, runExceptT, MonadError (throwError))
import Mapping

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
  -- submitEvalRes res
  evaluateMany xs

-- submitEvalRes :: ReturnType -> PContext ()
-- submitEvalRes (Res n) = void $ addNewNode n
-- submitEvalRes _ = return ()

evalWithEnv ::  Environment -> Command -> Either String (ReturnType, Environment)
evalWithEnv env com = runExcept $ runStateT (evaluate com) env

updateState :: Context -> Environment -> Either String Environment
updateState stateTrans env = 
  case runExcept $ runStateT stateTrans env of
    Left msg -> Left msg
    Right (_, env') -> Right env'
    

-- >>> evalWithEnv ([], 0) (parse "A := Set")
-- (Res (Class {tags = ["Set"], key = Exist {nameOf = "A", id = "0"}}),([],1))

evalExpr :: MathExp -> PContext Node
evalExpr (Apply1 (Symbol name) exp1) = do
  arg <- evalExpr exp1
  f' <- findByNameM' name
  f <- case f' of
        Mapping {} -> return f'
        _ -> throwError "evalExpr Apply1: f is not a function"
  -- isValid <- liftM2 (||) (arg `isInB` domain f) (arg `isSubsetOfB` domain f)
  -- if isValid then return (range f)
  -- else throwError "evalExpr Apply1: arg is not in domain"
  applyArg f' arg
evalExpr (Apply2 (Symbol name) exp1 exp2) = do
  arg1 <- evalExpr exp1
  arg2 <- evalExpr exp2
  f' <- findByNameM' name
  f <- case f' of
        Mapping {} -> return f'
        _ -> throwError "evalExpr Aply1: f is not a function"
  tup <- arg1 <.> arg2
  --isValid <- liftM2 (||) (arg1 `isSubsetOfB` ()) (arg2 `isSubsetOfB` exp2)
  applyArg f tup
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
  -- error $ show set'
  let res = Object key
  claim <- res `isIn` set'
  addNewNode claim
  -- error $ show claim
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

findByNameM' :: String -> PContext Node
findByNameM' name = do
  node <- findByNameM name
  case node of
    Nothing -> throwError $ "findByNameM': " ++ name ++ " not defined"
    Just n -> return n

-- findInClosure :: String -> Closure -> Maybe DefEntry
-- findInClosure name (x@(DefEntry sym _ _) : xs)
--   | sym == name = Just x
--   | otherwise = findInClosure name xs
-- findInClosure name [] = Nothing
