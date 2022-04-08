module Main where

import Control.Monad.Random.Strict
import Common
import Data.UUID

main :: IO ()
main = do
  g <- getStdGen  -- get a timestamp-seeded generator
  let log = evalRand app g  -- run the (pure) application in the monad
  putStr log

-- monad for the application
type M = Rand StdGen

-- the "pure" application, running in monad "M"
app :: M String
app = do
  foo <- myType "foo"
  bar <- myType "bar"
  -- do some processing
  return $ unlines ["Results:", show foo, show bar]

type AnotherType = String
data MyType = MyType {
   uuid :: UUID,
   elements :: AnotherType
} deriving (Show)

-- smart constructor for MyType with unique UUID
myType :: AnotherType -> M MyType
myType x = (MyType <$> getRandom) <*> (pure x)
