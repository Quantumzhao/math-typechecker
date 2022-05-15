module Main where

import ContextState
import Interpreter.Parser
import Printer.Print2String
import Printer.Format
import Node
import Control.Monad.State
import Interpreter.AST
import System.IO
import Data.Functor
import Control.Monad.Except
import Interpreter.Evaluator
import Relation

main :: IO ()
main = do
  contents <- loadFiles include
  case updateState (initRelations >> loadMany contents) initNodes of
    Left msg -> print msg >> putStrLn "Program Halt. "
    Right env' -> putStrLn "Successfully loaded all definitions" >> repl env'
  -- Right (ret, env) <- loadFiles include initNodes
  -- outputRes ret initNodes
  -- repl env

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
outputRes (Res n) env = do
  case runExcept $ formatNode n (fst env) of
    Right n -> printLns $ printExpr n
    Left msg -> print msg
outputRes Halt _ = return ()

-- >>> fst $ evalWithEnv ([], 0) (parse "A := Set A")
-- Res (Class {tags = ["Set"], key = Exist {nameOf = "A", id = "0"}})

printLns :: [String] -> IO ()
printLns (x : xs) = do
  putStrLn x
  printLns xs
printLns [] = return ()

initRelations :: PContext ()
initRelations = do
  addIsInRel
  addIsSubsetRel

-- load :: Environment -> String -> Either String (ReturnType, Environment)
-- load env content =
--   let inputs =  getAllParsed content in
--   runExcept $ runStateT (evaluateMany (Definition <$> inputs)) env

-- loadFiles :: [String] -> Environment -> IO (Either String (ReturnType, Environment))
-- loadFiles [] env = return $ Right (Halt, env)
-- loadFiles [x] env = (openFile x ReadMode >>= hGetContents) <&> load env
-- loadFiles (x : xs) env =
--   do
--     Right env' <- loadFiles [x] env
--     loadFiles xs (snd env')
loadContent :: String -> Context
loadContent content = do--void (evaluateMany (getAllParsed content))
  let c' = getAllParsed content
  -- error $ show c'
  evaluateMany c'
  return ()

loadMany :: [String] -> Context
loadMany [] = return ()
loadMany (x : xs) = do
  loadContent x
  loadMany xs

-- getContent path = 
--   let read = openFile path ReadMode >>= hGetContents in
--   read

-- loadFiles :: [String] -> Environment -> IO (Either String (ReturnType, Environment))
-- loadFiles [] env = return $ Right (Halt, env)
-- loadFiles [x] env = do
--   content <- openFile x ReadMode >>= hGetContents
--   return $ runExcept $ runStateT (load content) env
-- loadFiles (x : xs) env = do
--   Right env' <- loadFiles [x] env
--   loadFiles xs (snd env')

{-| paths -> contents -}
loadFiles :: [String] -> IO [String]
loadFiles [] = return []
loadFiles (x : xs) = do
  x' <- openFile x ReadMode >>= hGetContents
  xs' <- loadFiles xs
  return $ x' : xs'
