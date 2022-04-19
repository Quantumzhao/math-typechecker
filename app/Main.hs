module Main where

import ContextState
import Interpreter.Evaluate
import Interpreter.Parser
import Printer.Print2String

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
    Res output -> do
      printLns (printExpr output)
      repl env'
    Halt -> return ()

printLns :: [String] -> IO ()
printLns (x : xs) = do
  putStrLn x
  printLns xs
printLns [] = return ()
