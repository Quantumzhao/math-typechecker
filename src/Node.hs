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
  = Binary {
    lValue :: Node,
    rValue :: Node,
    out :: Node,
    tags :: [String]
  }
  | Unary {
    from :: UpperStructure,
    value :: Node, 
    out :: Node,
    tags :: [String]
  }
  | Set {
    defOf :: ElementTemplate,
    tags :: [String]
  }
  | Tuple {
    entries :: [Node],
    tags :: [String]
  }
  | Unit {
    tags :: [String],
    id :: Int
  } 
  | Relation {
    lValue :: Node,
    rValue :: Node,
    tags :: [String]
  }
  | Class {
    from :: UpperStructure,
    value :: Node,
    tags :: [String]
  }
  | Statement {
    from :: UpperStructure,
    tags :: [String]
  }
  deriving (Eq)

data UpperStructure
  = App {
    transformation :: Node,
    fixed :: Node
  }
  | Definition
  deriving (Eq)

data ElementTemplate 
  -- create from a defined set of elements
  = Collection [Node]
  -- create from expression
  | Type Node
  | Universal
  | Empty
  deriving (Eq)
