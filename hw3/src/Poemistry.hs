module Poemistry where

poemistry :: [Char] -> Integer -> [Char]
poemistry strlist k = let newstr = strlist in getPoemistry newstr k 0


prettyPrint :: [Char] -> [Char]
prettyPrint [] = []
prettyPrint (x1:x2:x3:x4:x5:xs) = (x1:x2:x3:x4:x5:'\n':(prettyPrint xs))

-- sort :: Ord a => [a] -> [a]
-- sort [] = []
-- sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]

getPoemistry :: [Char] -> Integer -> Integer -> [Char]
getPoemistry strlist k i | i < 20 = let w = toInteger $ (length strlist)^(19-i) in
                                        ((getCharInStr strlist (k `div` w)) : (getPoemistry strlist (k `mod` w) (i+1)))
                         | otherwise = []

getCharInStr :: [Char] -> Integer -> Char
getCharInStr [] k = error "string is empty!"
getCharInStr (c:cs) k | k == 0 = c
                 | k > 0 = getCharInStr cs (k-1)
                 | otherwise = error "k < 0!"
