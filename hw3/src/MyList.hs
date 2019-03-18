module MyList where

import Prelude hiding (concat)

data List a = a :~ (List a) | Nil
    deriving (Show)
infixr 5:~

concat :: List (List a) -> List a
concat Nil = Nil
concat (Nil:~li) = concat li
concat (x:~li) = concat2 x (concat li)

concat2 :: List a -> List a -> List a
concat2 x Nil = x
concat2 Nil y = y
concat2 x y = concat2 (myinit x) ((mylast x):~y)

myinit :: List a -> List a
myinit Nil = Nil
myinit (_:~Nil) = Nil
myinit (x:~y) = x:~(myinit y)

mylast :: List a -> a
mylast Nil = error "empty list!"
mylast (x:~Nil) = x
mylast (_:~y) = mylast y
