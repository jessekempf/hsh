{-# OPTIONS_GHC -Wall #-}
module HSH.DataStructures.PrefixTree where

import Prelude hiding (lookup)

import qualified Data.Map as Map

data PrefixTree v = Node { nodeValue :: Maybe v, children :: Map.Map Char (PrefixTree v) } deriving (Show, Eq)

empty :: PrefixTree v
empty = Node { nodeValue = Nothing, children = Map.empty }

singletonTree :: String -> v -> PrefixTree v
singletonTree [] value = Node (Just value) Map.empty
singletonTree (khead:krest) value =
  Node Nothing child
  where
    child = Map.singleton khead (singletonTree krest value)

insert :: String -> v -> PrefixTree v -> PrefixTree v
insert [key] value oldtree =
  oldtree { children = new_children }
  where
    new_children = Map.insert key (Node (Just value) Map.empty) (children oldtree)
insert (khead:krest) value oldtree =
  oldtree { children = Map.insert khead new_subtree (children oldtree) }
  where
    new_subtree = case Map.lookup khead (children oldtree) of
                    Just subtree -> insert krest value subtree
                    Nothing -> singletonTree krest value

lookup :: String -> PrefixTree v -> Maybe v
lookup [] tree =
  nodeValue tree
lookup (khead:krest) tree = do
  subtree <- Map.lookup khead (children tree)
  lookup krest subtree
