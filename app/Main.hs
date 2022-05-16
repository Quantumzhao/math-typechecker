module Main( main ) where

import ContextState ( PContext, Environment, Context )
import Interpreter.Parser ( getAllParsed, parse )
import Printer.Print2String ( printExpr )
import Printer.Format ( formatNode )
import Control.Monad.State ( void, StateT(runStateT) )
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Control.Monad.Except ( runExcept, MonadError(throwError) )
import Interpreter.Evaluator ( evaluateMany, evalWithEnv, updateState, ReturnType(..) )
import Relation ( addIsSubsetRel, addIsInRel )
import Set ( addEmpty, addUniverse )

main :: IO ()
main = do
  paths <- getDeps
  contents <- loadFiles paths
  -- first load in the hard coded relations (that are hard to define using this DSL)
  -- then the basic definitions from external files
  case updateState (initNodes >> loadMany contents) initialState of
    Left msg -> putStrLn msg >> putStrLn "Program Halt. "
    Right env' -> putStrLn "Successfully loaded all definitions" >> repl env'

initialState = ([], 0)

configPath = "./app/deps.cfg"

-- the main loop
repl :: Environment -> IO ()
repl env = do
  input <- getLine
  -- first parse the input
  case parse input of
    Left msg -> void $ print msg
    Right parsed -> 
      -- then evaluate to evolve the state (environment)
      case evalWithEnv env parsed of
        -- stop the program
        Right (Halt, _) -> return ()
        -- output the result
        Right t@(res, env') -> outputRes res env' >> repl env'
        -- print out error message
        Left msg -> putStrLn msg >> repl env

outputRes :: ReturnType -> Environment -> IO ()
outputRes (Res n) env = do
  case runExcept $ runStateT (formatNode n) env of
    -- if the result is a node, do the preprocessing to generate an output expression, similar to AST
    Right (n, _) -> printLns $ printExpr n
    Left msg -> print msg
outputRes Halt _ = return ()

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

loadContent :: String -> Context
loadContent content = do
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

{-| paths -> contents -}
-- takes in a list of paths of files, and get the contents from the files
loadFiles :: [String] -> IO [String]
loadFiles [] = return []
loadFiles (x : xs) = do
  x' <- openFile x ReadMode >>= hGetContents
  xs' <- loadFiles xs
  return $ x' : xs'

getDeps :: IO [String]
getDeps = do
  content <- openFile configPath ReadMode >>= hGetContents
  return $ lines content
