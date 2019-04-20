import Test.QuickCheck
import PriorityQueue(PriorityQueue(..))
import BHeap(BHeap(..))
-- You may import more
import Data.List (sort)

main :: IO ()
main = do
    -- example, weak properties
    quickCheck prop_1_empty_is_empty
    quickCheck prop_2_findMin_the_only_element
    quickCheck prop_3_tautology
    -- my properties
    quickCheck prop_4_findMin_simple
    quickCheck prop_5_findMin_advanced
    quickCheck prop_6_meld
    quickCheck prop_7_test_is_empty

    -- quickCheck or verboseCheck more properties here!

-- 1. Empty queue should be empty
prop_1_empty_is_empty :: Bool
prop_1_empty_is_empty = isEmpty empty_BHeap_of_Integer

empty_BHeap_of_Integer :: BHeap Integer
empty_BHeap_of_Integer = empty

-- 2. For all integer n, insert n to an empty priority queue, then findMin from it, the result should be n
prop_2_findMin_the_only_element :: Integer -> Bool
prop_2_findMin_the_only_element n = findMin s == n where
    s = insert n empty_BHeap_of_Integer

-- 3. For all integer n, for all non-empty heap h, either n <= findMin h or n > findMin h.
-- This is a taotology, only to demostrate how to write a property containing implication (==>) and multiple random inputs.
prop_3_tautology :: Integer -> BHeap Integer -> Property
prop_3_tautology n h = not (isEmpty h) ==>
    n <= findMin h || n > findMin h

-- | Generator of @BHeap a@, used to generate random @BHeap@s
instance (Arbitrary a, Ord a) => Arbitrary (BHeap a) where
    arbitrary = do
        avs <- arbitrary -- :: Gen [a] -- see also @vector :: Arbitrary a => Int -> Gen [a]@ 
        return (fromList avs)


prop_4_findMin_simple :: [Integer] -> Property
prop_4_findMin_simple xs = not (null xs) ==>
    minimum xs == (findMin $ (fromList xs :: BHeap Integer))

prop_5_findMin_advanced :: [Integer] -> Bool
prop_5_findMin_advanced xs = sort xs == toList (fromList xs :: BHeap Integer)

prop_6_meld :: [[Integer]] -> Bool
prop_6_meld xs = sort (concat xs) == toList (foldl meld empty_BHeap_of_Integer (map fromList xs))

prop_7_test_is_empty :: [Integer] -> Bool
prop_7_test_is_empty xs = null xs == isEmpty (fromList xs :: BHeap Integer)