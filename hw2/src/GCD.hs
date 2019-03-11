module GCD
    ( myGCD
    ) where

myGCD :: Integer->Integer->Integer
myGCD a 0 = abs a
myGCD 0 b = abs b
myGCD a b = abs (myGCD b (mod a b))
