module Interpreter.Evaluate where
import ContextState
import Control.Monad.State.Lazy
import Printer.FormatDef hiding (Clause)
import Interpreter.AST
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
  case body of
    ToCollection colDef -> genCollectionDef closure colDef
    ToOther (MappingDef {}) -> undefined
    ToOther (RelDef name from to tags) -> undefined
    ToOther (ObjectDef name set) -> genObjectDef closure name set
    ToOther (OtherVariable varName) -> undefined

genCollectionDef :: Closure -> Collection -> PContext Node
genCollectionDef closure collection@(CollectionDef name tags body) = do
  id <- getNewId
  def <- genCollectionBody closure body
  return $ Collection {
    defOf = def,
    Node.tags = tags,
    key = Unique name id
  }
genCollectionDef _ (CVariableRep'n name) = do undefined
  -- nodes <- getNodes
  -- case findInClosure name  of
  --   Just def -> undefined 
  --   Nothing -> 
  --     case findFirst (\x -> nameOf (key x) == name) nodes of
  --       Just node -> do
  --         undefined 
  --       Nothing -> error $ "genCollectionDef: " ++ name ++ " not found"

genCollectionBody :: Closure -> SetBodyDef -> PContext ElementTemplate
genCollectionBody closure bodyDef =
  undefined

genObjectDef :: Closure -> String -> Collection -> PContext Node
genObjectDef closure name set =
  case set of
    CollectionDef {} -> error "not implemented"
    CVariableRep'n setName -> do
      nodes <- getNodes
      id <- getNewId
      let object = Object (Unique name id)
      case findFirst (\n -> nameOf (key n) == setName) nodes of
        Nothing -> error "genObjectDef: set not defined"
        Just set -> do 
          addNewStatementM (object `isIn` set)
          return object

genRelationDef closure name rel = undefined 


findInClosure :: String -> Closure -> Maybe DefEntry
findInClosure name BlankClosure = Nothing
findInClosure name (Clause (x@(DefEntry sym _ _) : xs))
  | sym == name = Just x
  | otherwise = findInClosure name (Clause xs)
findInClosure name (Clause []) = Nothing

