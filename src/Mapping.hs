module Mapping where

import Set
import Node
import Tags

automorphism :: Class -> Class
automorphism s
  | isSet s = s
  | otherwise = error "Mapping.automorphism: not a set"
