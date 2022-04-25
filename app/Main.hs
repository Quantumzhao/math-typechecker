module Main where

import ContextState
import Interpreter.Evaluate
import Interpreter.Parser
import Printer.Print2String
import Printer.Format (formatNode)
import Node

main :: IO ()
main =  repl ([], 0)

repl :: GraphI -> IO ()
repl env = do
  input <- getLine
  let (res, env') = evalWithEnv env (parse input)
  case res of
    Err errMsg -> do 
      putStrLn errMsg
      repl env
    Res n -> do
      printLns $ printExpr $ formatNode n (fst env')
      repl env'
    Halt -> return ()

printLns :: [String] -> IO ()
printLns (x : xs) = do
  putStrLn x
  printLns xs
printLns [] = return ()
