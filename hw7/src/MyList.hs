module MyList where

import MyListType
-- 1. Eq
instance Eq a => Eq (List a) where
    Nil == Nil = True
    (x:~xs) == (y:~ys) = x == y && xs == ys
    _ == _ = False

-- 2. Ord
instance Ord a => Ord (List a) where
    compare x y = if x == y then EQ
                else if x <= y then LT
                else GT
    -- define <
    Nil < Nil = False
    Nil < _ = True
    (x:~xs) < (y:~ys) = x < y || (x == y && xs < ys)
    _ < _ = False
    -- define <=
    x <= y = x < y || x == y

-- 3. Show
instance Show a => Show (List a) where
    show Nil = "[]"
    show (x:~xs) = ('[' : show x) ++ show' xs ++ "]"
        where show' Nil = ""
              show' (x:~xs) = ',' : show x ++ show' xs