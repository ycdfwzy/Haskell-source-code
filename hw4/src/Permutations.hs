module Permutations where

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat [ins x p | p <- perms xs]

ins :: a -> [a] -> [[a]]
ins x [] = [[x]]
ins x (p:ps) = (x:p:ps) : [p:q | q <- ins x ps]
