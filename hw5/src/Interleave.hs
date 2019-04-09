module Interleave where

-- a little TLE(about 5s) for extreme tests
interleave :: [a] -> [a] -> [a]
interleave [] y = y
interleave x [] = x
interleave (x:xs) (y:ys) = x:(y:interleave xs ys)

interleaveLists :: [[a]] -> [a]
interleaveLists = go 0 0
    where go n k li | n==k = (li !! n !! 0) : go (n+1) 0 li
                    | otherwise = (li !! k !! (n-k)) : go n (k+1) li


-- awesome method
interleave' :: [a] -> [a] -> [a]
interleave' [] y = y
interleave' (x:xs) y = x:interleave' y xs

interleaveLists' :: [[a]] -> [a]
interleaveLists' = foldr interleave' []


-- tricky
-- interleave' :: [a] -> [a] -> [a]
-- interleave' x y = foldr (:) (take (10^7) x) (take (10^7) y)