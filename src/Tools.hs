-- Collection of safe version of standard functions

module Tools 
where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Types

-- head is dangerous
stringHead :: [String] -> String
stringHead [] = ""
stringHead xs = head xs

-- tail is dangerous....
safeTail :: [a] -> [a]
safeTail []   = []
safeTail xs = tail xs

-- convert EStatus into a string with some decent formatting
ppEStatus :: EStatus -> String
ppEStatus status = Map.foldrWithKey ppmap "" $ fromEStatus status

-- function given to the foldrWithKey
ppmap :: Tag -> (Set.Set EMail) -> String -> String
ppmap k v acc = acc ++ k ++ ":\n" ++ (pplist (Set.toList v) "    ")

-- convert a set into a string with 5 email in each row
pplist :: [EMail] -> String -> String
pplist set indent = case splitAt 4 $ set of
                   (xs, []) -> indent ++ (group xs) ++ "\n"
                   (xs, rest) -> indent ++ (group xs) ++ "\n" ++ (pplist rest indent)
                   where group = concat . List.intersperse ", "
