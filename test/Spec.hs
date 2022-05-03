import Prelude
import Control.Monad.State.Lazy
import ContextState
import Node
import Set

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented"

-- >>> dummy [1, 2, 3] [1, 2, 3]
-- [2,3,4,3,4,5,4,5,6]

dummy :: [Int] -> [Int] -> [Int]
dummy xs ys = do
  x <- xs
  y <- ys
  return $ x + y

