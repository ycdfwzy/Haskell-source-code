module Distance
    ( solution
    ) where

solution :: Integer -> (Double, Double) -> (Double, Double) -> Double
solution p p1 p2 = case p of
    1 -> manhattan p1 p2
    2 -> euler p1 p2
    _ -> chebyshev p1 p2

manhattan :: (Double, Double) -> (Double, Double) -> Double
manhattan = \p1 p2 -> let (x1,y1)=p1 in let (x2,y2)=p2 in (abs (x1-x2)) + (abs (y1-y2))
euler :: (Double, Double) -> (Double, Double) -> Double
euler = \p1 p2 -> let (x1,y1)=p1 in let (x2,y2)=p2 in sqrt ((x1-x2)^2 + (y1-y2)^2)
chebyshev :: (Double, Double) -> (Double, Double) -> Double
chebyshev = \p1 p2 -> let (x1,y1)=p1 in let (x2,y2)=p2 in max (abs (x1-x2)) (abs (y1-y2))