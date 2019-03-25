module Collatz where

collatzs :: Integer -> [Integer]
collatzs x | x == 1 = [x]
           | x > 0 = x : (collatzs . collatzFunc $ x)
           | otherwise = error "x should be positive"

collatzFunc :: Integer -> Integer
collatzFunc x | x `mod` 2 == 0 = x `div` 2
              | otherwise = 3*x+1
