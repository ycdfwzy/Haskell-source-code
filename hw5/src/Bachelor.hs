module Bachelor where

import Data.Bits (xor)

bachelor :: [Integer] -> Integer
bachelor = foldl xor 0