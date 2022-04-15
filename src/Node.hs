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
  | Object {
    from :: UpperStructure,
    tags :: [String]
  }
  | Relation {
    lValue :: Node,
    rValue :: Node,
    tags :: [String]
  }
  | FixedRelation {
    from :: UpperStructure,
    value :: Node,
    tags :: [String]
  }
  | Statement {
    from :: UpperStructure,
    tags :: [String]
  }

data UpperStructure
  = App {
    transformation :: Node,
    fixed :: Node
  }
  | Definition

-- areSets :: [Node] -> Bool
-- areSets = all matchSet
--   where matchSet (Set _) = True
--         matchSet _ = False
