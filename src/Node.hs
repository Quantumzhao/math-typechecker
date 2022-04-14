module Node where

data Node
  = Element {
    id :: Int
  }
  | Set {
    name :: String,
    id :: Int
  }
  | Group {
    name :: String,
    id :: Int
  }
  deriving (Eq, Show)

areSets :: [Node] -> Bool
areSets = all matchSet
  where matchSet (Set _ _) = True
        matchSet _ = False
