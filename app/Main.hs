module Main where

import ContextState
import Interpreter.Evaluator
import Interpreter.Parser
import Printer.Print2String
import Printer.Format
import Node
import Control.Monad.State
import Interpreter.AST
import System.IO
import Data.Functor

main :: IO ()
main = do
  (ret, env) <- loadFiles include initGraph
  outputRes ret initGraph
  repl env

initGraph :: (Graph, Int)
initGraph = ([], 0)

include :: [String]
include = ["./examples/test.mathdef"]

repl :: GraphI -> IO ()
repl env = do
  input <- getLine
  let t@(res, env') = evalWithEnv env (parse input)
  outputRes res env'
  if res == Halt then return ()
  else repl (updateEnv t)

outputRes :: ReturnType -> GraphI -> IO ()
outputRes (Err msg) _ = putStrLn msg
outputRes (Res n) env = printLns $ printExpr $ formatNode n (fst env)
outputRes Halt _ = return ()

updateEnv :: (ReturnType, GraphI) -> GraphI
updateEnv (Res r, (ns, i)) = (r : ns, i)
updateEnv (_, g) = g

-- >>> fst $ evalWithEnv ([], 0) (parse "A := Set A")
-- Res (Class {tags = ["Set"], key = Exist {nameOf = "A", id = "0"}})

printLns :: [String] -> IO ()
printLns (x : xs) = do
  putStrLn x
  printLns xs
printLns [] = return ()

load :: GraphI -> String -> (ReturnType, GraphI)
load env content =
  let inputs =  getParsedDefs content in
  runState (evaluateMany (Definition <$> inputs)) env

loadFiles :: [String] -> GraphI -> IO (ReturnType, GraphI)
loadFiles [] env = return (Halt, env)
loadFiles [x] env = (openFile x ReadMode >>= hGetContents) <&> load env
loadFiles (x : xs) env = loadFiles [x] env >>= loadFiles xs . snd
