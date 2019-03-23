{-# OPTIONS_GHC -F -pgmF htfpp #-}
import qualified MyIntList
import qualified MyList
import qualified Poemistry

import Test.Framework
import System.Timeout(timeout)

-- TODO in test report, Failure, not expected timeout 2019-03-06
addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06

main = htfMain testsWithTimeouts

closeEnough :: Double -> Double -> Bool
closeEnough actual expect = abs (actual-expect) < 1e-6


-- template code
-- Problem 1
myIntListToList MyIntList.Nil = []
myIntListToList (MyIntList.Cons x xs) = x:(myIntListToList xs)

-- Problem 3
poemistry :: [Char] -> Integer -> [Char]
poemistry dict k = poemistryAcc dict (toInteger $ length dict) k [] 0

poemistryAcc :: [Char] -> Integer -> Integer -> [Char] -> Integer -> [Char]
poemistryAcc dict dictSize k acc 20 = acc
poemistryAcc dict dictSize k acc accSize = poemistryAcc dict dictSize (div k dictSize) (dict !! fromIntegral (mod k dictSize) : acc) (accSize + 1)

prettyPrint :: [Char] -> [Char]
prettyPrint [] = []
prettyPrint xs = (take 5 xs) ++ ('\n' : (prettyPrint $ drop 5 xs))

-- assertEqual as preprocessor macro https://hackage.haskell.org/package/HTF-0.13.2.2/docs/Test-Framework-HUnitWrapper.html
test_1_1 = assertEqual 1 (MyIntList.length (MyIntList.Cons 1 MyIntList.Nil))
test_1_2 = assertEqual 0 (MyIntList.length MyIntList.Nil)
test_1_3 = assertEqual [] (myIntListToList $ MyIntList.tail (MyIntList.Cons 1 MyIntList.Nil))
test_1_4 = assertEqual 2 (MyIntList.last (MyIntList.Cons 1 (MyIntList.Cons 2 MyIntList.Nil)))
test_1_5 = assertEqual 1 (MyIntList.head (MyIntList.Cons 1 (MyIntList.Cons 2 MyIntList.Nil)))
test_1_6 = assertEqual [1] (myIntListToList $ MyIntList.init (MyIntList.Cons 1 (MyIntList.Cons 2 MyIntList.Nil)))
test_1_7 = assertEqual [1] (myIntListToList $ MyIntList.take 1 (MyIntList.Cons 1 (MyIntList.Cons 2 MyIntList.Nil)))
test_1_8 = assertEqual [] (myIntListToList $ MyIntList.drop 10 MyIntList.Nil)



toMyList :: [a] -> MyList.List a
toMyList [] = MyList.Nil
toMyList (x:xs) = x MyList.:~ (toMyList xs)

fromMyList :: MyList.List a -> [a]
fromMyList MyList.Nil = []
fromMyList (x MyList.:~ xs) = x:(fromMyList xs)

list_2_1 = ['a', 'b']
list_2_2 = []
list_2_3 = ['c', 'd']
list_2_4 = list_2_1
list_2_5 = ['d'..'p']
ls_2_1 = []
ls_2_2 = [list_2_1]
ls_2_3 = [list_2_1, list_2_2]
ls_2_4 = [list_2_1, list_2_2, list_2_3]
ls_2_5 = [list_2_1, list_2_2, list_2_3, list_2_4]
ls_2_6 = take 10000 $ repeat list_2_5
ans_2_1 = concat ls_2_1
ans_2_2 = concat ls_2_2
ans_2_3 = concat ls_2_3
ans_2_4 = concat ls_2_4
ans_2_5 = concat ls_2_5
ans_2_6 = concat ls_2_6
test_2_1 = assertEqual ans_2_1 (fromMyList $ MyList.concat $ toMyList $ map toMyList (ls_2_1 :: [[Char]]))
test_2_2 = assertEqual ans_2_2 (fromMyList $ MyList.concat $ toMyList $ map toMyList ls_2_2)
test_2_3 = assertEqual ans_2_3 (fromMyList $ MyList.concat $ toMyList $ map toMyList ls_2_3)
test_2_4 = assertEqual ans_2_4 (fromMyList $ MyList.concat $ toMyList $ map toMyList ls_2_4)
test_2_5 = assertEqual ans_2_5 (fromMyList $ MyList.concat $ toMyList $ map toMyList ls_2_5)
test_2_6 = assertEqual ans_2_6 (fromMyList $ MyList.concat $ toMyList $ map toMyList ls_2_6)

-- Problem 3
dict_3_1 = ['白', '日', '依', '山', '尽']
dict_3_2 = take 100 $ concat $ repeat ['A'..'Z']
dict_3_3 = take 1000 $ concat $ repeat ['A'..'Z']
ans_3_1 = poemistry dict_3_1 10
ans_3_2 = poemistry dict_3_2 40

test_3_1 = assertEqual (poemistry dict_3_1 10) (Poemistry.poemistry dict_3_1 10)
test_3_2 = assertEqual (poemistry dict_3_2 40) (Poemistry.poemistry dict_3_2 40)
test_3_3 = assertEqual (poemistry dict_3_1 100) (Poemistry.poemistry dict_3_1 100)
test_3_4 = assertEqual (poemistry dict_3_3 10000000) (Poemistry.poemistry dict_3_3 10000000)
test_3_5 = assertEqual (prettyPrint ans_3_1) (Poemistry.prettyPrint ans_3_1)
test_3_6 = assertEqual (prettyPrint ans_3_2) (Poemistry.prettyPrint ans_3_2)
