module Brute where

getPINs :: String -> [String]
getPINs observed | observed == "" = []
                 | otherwise = work observed

work :: String -> [String]
work [] = [""]
work (c : s) | c == '1' = stringConcat "124" $ work s
             | c == '2' = stringConcat "1235" $ work s
             | c == '3' = stringConcat "236" $ work s
             | c == '4' = stringConcat "1457" $ work s
             | c == '5' = stringConcat "24568" $ work s
             | c == '6' = stringConcat "3569" $ work s
             | c == '7' = stringConcat "478" $ work s
             | c == '8' = stringConcat "05789" $ work s
             | c == '9' = stringConcat "689" $ work s
             | c == '0' = stringConcat "08" $ work s
             | otherwise = error "unknown char"

stringConcat :: String -> [String] -> [String]
stringConcat [] _ = []
stringConcat (c : s) sl = map ((\x y -> (x:y)) c) sl ++ stringConcat s sl
