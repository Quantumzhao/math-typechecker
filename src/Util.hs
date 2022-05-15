module Util where
import qualified Data.Char as C

toLower :: String -> String
toLower (x : xs) = C.toLower x : xs
toLower [] = []