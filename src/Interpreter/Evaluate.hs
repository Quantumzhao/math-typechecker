module Interpreter.Evaluate where
import ContextState
import Control.Monad.State.Lazy
import Printer.FormatDef
import Interpreter.AST

data ReturnType
    = Res Expr
    | Err String
    | Halt

evaluate :: Command -> PContext ReturnType
evaluate com = 
  case com of
    Definition def -> do
      res <- evalDefinition def
      return $ Res res
    Info symbol -> evalInfo symbol
    Exit -> return Halt
    AnonymousExpr expr -> evalAnonymousExpr expr

evalWithEnv ::  GraphI -> Command -> (ReturnType, GraphI)
evalWithEnv env com = runState (evaluate com) env

evalInfo symbol = undefined 

evalAnonymousExpr expr = undefined 

evalDefinition :: DefEntry -> PContext Expr
evalDefinition (DefEntry name body closure) = do
  case body of
    ToSet setDef -> undefined 
    ToOther (MappingDef {}) -> undefined
    ToOther (RelDef {}) -> undefined
    ToOther (ObjectDef {}) -> undefined
    ToOther (OtherVariable varName) -> undefined 
