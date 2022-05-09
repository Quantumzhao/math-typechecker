module Main where

import ContextState
import Interpreter.Evaluator
import Interpreter.Parser
import Printer.Print2String
import Printer.Format (formatNode)
import Node
import Control.Monad.State
import Interpreter.AST (Command(Definition))
import System.IO

main :: IO ()
main = do
  hd <- openFile "./examples/test.mathdef" ReadMode
  contents <- hGetContents hd
  let (ret, env) = load contents initGraph
  outputRes ret initGraph
  repl env

initGraph :: (Graph, Int)
initGraph = ([], 0)

repl :: GraphI -> IO ()
repl env = do
  input <- getLine
  let (res, env') = evalWithEnv env (parse input)
  outputRes res env'
  if res == Halt then return ()
  else repl env'

outputRes :: ReturnType -> GraphI -> IO ()
outputRes (Err msg) _ = putStrLn msg
outputRes (Res n) env = printLns $ printExpr $ formatNode n (fst env)
outputRes Halt _ = return ()

-- >>> fst $ evalWithEnv ([], 0) (parse "A := Set A")
-- No instance for (Show ReturnType) arising from a use of ‘evalPrint’

printLns :: [String] -> IO ()
printLns (x : xs) = do
  putStrLn x
  printLns xs
printLns [] = return ()

load :: String -> GraphI -> (ReturnType, GraphI)
load content env = 
  let inputs =  getParsedDefs content in
  runState (evaluateMany (Definition <$> inputs)) env
