module TriangleTrans where

import MyLens
import TriangleType

coordXLens f (Point a b) = fmap (\a' -> (Point a' b)) (f a)
coordYLens f (Point a b) = fmap (\b' -> (Point a b')) (f b)
triALens f (Triangle a b c) = fmap (\a' -> (Triangle a' b c)) (f a)
triBLens f (Triangle a b c) = fmap (\b' -> (Triangle a b' c)) (f b)
triCLens f (Triangle a b c) = fmap (\c' -> (Triangle a b c')) (f c)
