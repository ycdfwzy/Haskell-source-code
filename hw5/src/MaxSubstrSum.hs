module MaxSubstrSum where

solution :: [Integer] -> Integer
solution arg = foldr (\(a, b) x -> max x (a-b)) 0 $ scanl (\(a, b) y -> (a+y, min (a+y) b)) (0, 0) arg