module LCM
    ( solution
    ) where

solution :: Integer -> Integer -> Integer
solution a b = a*b `div` (gcd a b)