module Prefix where

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix xl yl = (sum $ zipWith (\x y -> if x == y then 0 else 1) xl yl) == 0