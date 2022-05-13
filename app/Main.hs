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
import Control.Monad.Except

main :: IO ()
main = do
  Right (ret, env) <- loadFiles include initNodes
  outputRes ret initNodes
  repl env
  repl initNodes

initNodes = ([], 0)

include = ["./examples/test.mathdef"]

repl :: Environment -> IO ()
repl env = do
  input <- getLine
  case evalWithEnv env (parse input) of
    Right (Halt, _) -> return ()
    Right t@(res, env') -> outputRes res env' >> repl env'
    Left msg -> putStrLn msg >> repl env

outputRes :: ReturnType -> Environment -> IO ()
outputRes (Res n) env = printLns $ printExpr $ formatNode n (fst env)
outputRes Halt _ = return ()

-- updateEnv :: (ReturnType, Environment) -> Environment
-- updateEnv (Res r, (ns, i)) = (r : ns, i)
-- updateEnv (_, g) = g

-- >>> fst $ evalWithEnv ([], 0) (parse "A := Set A")
-- Res (Class {tags = ["Set"], key = Exist {nameOf = "A", id = "0"}})

printLns :: [String] -> IO ()
printLns (x : xs) = do
  putStrLn x
  printLns xs
printLns [] = return ()

load :: Environment -> String -> Either String (ReturnType, Environment)
load env content =
  let inputs =  getParsedDefs content in
  runExcept $ runStateT (evaluateMany (Definition <$> inputs)) env

loadFiles :: [String] -> Environment -> IO (Either String (ReturnType, Environment))
loadFiles [] env = return $ Right (Halt, env)
loadFiles [x] env = (openFile x ReadMode >>= hGetContents) <&> load env
loadFiles (x : xs) env =
  do
    Right env' <- loadFiles [x] env
    loadFiles xs (snd env')
