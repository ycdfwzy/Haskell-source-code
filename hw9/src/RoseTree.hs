module RoseTree where

import RoseTreeType

instance Functor RoseTree where
    fmap f (Node a b) = Node (f a) $ map (fmap f) b
