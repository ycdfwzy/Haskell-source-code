module BinaryBalance where

import BinTree

isBalance :: BinTree.BinTree a -> Bool
isBalance Nil = True
isBalance (Node leftNode rightNode a) = let (_, x)=checkBalance (Node leftNode rightNode a) in x
    where
        checkBalance Nil = (0, True)
        checkBalance (Node leftNode rightNode a) = 
            let (left_h, left_flag) = checkBalance leftNode in
            let (right_h, right_flag) = checkBalance rightNode in
            (max left_h right_h + 1, left_flag && right_flag && (abs (left_h-right_h) < 2))

-- isBalance (Node leftNode rightNode a) = (isBalance leftNode) && (isBalance rightNode) && (abs ((height leftNode)-(height rightNode)) < 2)
