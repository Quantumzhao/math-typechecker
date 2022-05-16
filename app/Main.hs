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
import Set

main :: IO ()
main = do
  contents <- loadFiles include
  case updateState (initNodes >> loadMany contents) initialState of
    Left msg -> putStrLn msg >> putStrLn "Program Halt. "
    Right env' -> putStrLn "Successfully loaded all definitions" >> repl env'
  -- Right (ret, env) <- loadFiles include initNodes
  -- outputRes ret initNodes
  -- repl env

initialState = ([], 0)

include = ["./examples/test.mathdef"]

repl :: Environment -> IO ()
repl env = do
  input <- getLine
  case parse input of
    Left msg -> void $ print msg
    Right parsed -> 
      case evalWithEnv env parsed of
        Right (Halt, _) -> return ()
        Right t@(res, env') -> outputRes res env' >> repl env'
        Left msg -> putStrLn msg >> repl env

outputRes :: ReturnType -> Environment -> IO ()
outputRes (Res n) env = do
  case runExcept $ runStateT (formatNode n) env of
    Right (n, _) -> printLns $ printExpr n
    Left msg -> print msg
outputRes Halt _ = return ()

-- >>> fst $ evalWithEnv ([], 0) (parse "A := Set A")
-- Res (Class {tags = ["Set"], key = Exist {nameOf = "A", id = "0"}})

printLns :: [String] -> IO ()
printLns (x : xs) = do
  putStrLn x
  printLns xs
printLns [] = return ()

initNodes :: PContext ()
initNodes = do
  addUniverse
  addEmpty
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
  c' <- case getAllParsed content of
        Left msg -> throwError msg
        Right sth -> return sth
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
