module MyIntList where
import Prelude hiding (length, head, tail, init, last,take, drop)

data IntList = Cons Int IntList | Nil
    deriving (Show)

length :: IntList -> Int
length il = lenAcc il 0
    where lenAcc Nil n = n
          lenAcc (Cons _ il) n = lenAcc il (n+1)

head :: IntList -> Int
head (Cons a il) = a
head Nil = error "empty list!"

tail :: IntList -> IntList
tail Nil = Nil
tail (Cons _ il) = il

init :: IntList -> IntList
init Nil = Nil
init (Cons _ Nil) = Nil
init (Cons a il) = (Cons a (init il))

last :: IntList -> Int
last (Cons a Nil) = a
last (Cons _ il) = last il
last Nil = error "empty list!"

take :: Int -> IntList -> IntList
take n Nil = Nil
take n (Cons a il) | n <= 0 = Nil
                   | otherwise = (Cons a (take (n-1) il))

drop :: Int -> IntList -> IntList
drop n Nil = Nil
drop n il@(Cons _ il') | n <= 0 = il
                       | otherwise = drop (n-1) il'
