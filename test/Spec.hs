import Set
import Prelude
import Common
import Control.Monad.State.Lazy

main :: IO ()
main = do 
  putStrLn "Test suite not yet implemented"
  putStrLn test_Cross

test_Cross = 
  let a = evalState (getSimpleSet "A") [] in
  let b = evalState (getSimpleSet "B") [] in
  show $ cross [a, b]

