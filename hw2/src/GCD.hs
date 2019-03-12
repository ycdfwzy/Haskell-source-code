module GCD
    ( myGCD
    ) where

myGCD :: Integer->Integer->Integer
myGCD a b = myGCD_ (abs a) (abs b)
    where
        myGCD_ a 0 = a
        myGCD_ a b = myGCD_ b (rem a b)
-- myGCD a 0 = abs a
-- myGCD 0 b = abs b
-- myGCD a b = abs (myGCD b (mod a b))
