import Prelude
import Common
import Control.Monad.State.Lazy
import ContextState
import Node
import Set
import qualified Data.Map

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented"
  print test_Cross

test_Cross = 
  let state = do
              a <- getNewSet "A" []
              b <- getNewSet "B" []
              powerset a
  in evalState state (Data.Map.empty, 0)

