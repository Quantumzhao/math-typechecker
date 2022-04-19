module Interpreter.Evaluate where
import ContextState
import Control.Monad.State.Lazy
import Printer.FormatDef

data ReturnType
    = Res Expr
    | Err String
    | Halt

evaluate :: a -> PContext ReturnType
evaluate = undefined 

evalWithEnv ::  GraphI -> a -> (ReturnType, GraphI)
evalWithEnv env a = runState (evaluate a) env