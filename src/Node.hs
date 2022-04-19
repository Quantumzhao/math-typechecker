module Node where

-- data Node
--   = Element
--   | Set {
--     tags :: [String]
--   }
--   | Group {
--     tags :: [String]
--   }
--   | Map {
--     tags :: [String]
--   }
--   | Relation {
--     Node :: StructuralRelation,
--     tags :: [String]
--   }
--   | BinOp {
--     operation :: BinaryOperationType,
--     tags :: [String]
--   }
--   | Statement {
--     dependencies :: [Node]
--   }

-- data BinaryOperationType
--   = Intersect
--   | Union
--   | Cross
--   | RelCompl

-- data StructuralRelation
--   = Subset

data Node
  = Mapping {
    domain :: Node, 
    range :: Node,
    tags :: [String], 
    nameOf :: String, 
    id :: Int
  }
  | Set {
    defOf :: ElementTemplate,
    tags :: [String], 
    nameOf :: String, 
    id :: Int
  }
  | Tuple {
    entries :: (Node, Node),
    nameOf :: String, 
    id :: Int
  }
  -- the most generic type of objects
  | Object {
    nameOf :: String, 
    id :: Int
  } 
  | Relation {
    domain :: Node,
    codomain :: Node,
    tags :: [String], 
    nameOf :: String, 
    id :: Int
  }
  deriving (Eq, Show)

data UpperStructure
  = App {
    transformation :: Node,
    fixed :: Node
  }
  | Definition
  deriving (Eq, Show)

data ElementTemplate 
  -- create from a defined set of elements
  = Collection [Node]
  -- for all ... in ..., be in this form
  | FormOf Node
  | Universal
  | Empty
  deriving (Eq, Show)
