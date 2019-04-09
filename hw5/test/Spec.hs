{-# OPTIONS_GHC -F -pgmF htfpp #-}
import qualified Bachelor
import qualified Interleave
import qualified MaxSubstrSum

import Test.Framework
import System.Timeout(timeout)

import Data.List(elemIndex) -- simpler than findIndex
import Data.Maybe(fromJust)

-- TODO in test report, Failure, not expected timeout 2019-03-06
addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06

main = htfMain testsWithTimeouts

closeEnough :: Double -> Double -> Bool
closeEnough actual expect = abs (actual-expect) < 1e-6


-- assertEqual as preprocessor macro https://hackage.haskell.org/package/HTF-0.13.2.2/docs/Test-Framework-HUnitWrapper.html
test_1_1 = assertEqual 3 (Bachelor.bachelor [1,3,42,1,42])
test_1_2 = assertEqual 9 (Bachelor.bachelor [9])

xs1 = [1,2]
ys1 = [3,4]
x1 = 4
test_2_1 = assertEqual True $ fromJust (elemIndex x1 $ Interleave.interleave xs1 ys1) >= 0
xs2 = [1,2]
ys2 = [3..]
x2 = 10^7-3
test_2_2 = assertEqual True $ fromJust (elemIndex x2 $ Interleave.interleave xs2 ys2) >= 0
daytimes = ("D",1) : map (\(x,y)->(x,y+1)) daytimes
nights = ("N",1) : map (\(x,y)->(x,y+1)) nights
daysAndNights = Interleave.interleave daytimes nights 
x3 = ("D",10^6)
test_2_3 = assertEqual True $ fromJust (elemIndex x3 daysAndNights) >= 0

intPairs = Interleave.interleaveLists [ [ (x, y) | y <- [1..] ] | x <- [1..] ]
x4 = (5,5)
x5 = (10,10)
x6 = (13,13)
test_2_4 = assertEqual True $ fromJust (elemIndex x4 intPairs) >= 0
test_2_5 = assertEqual True $ fromJust (elemIndex x5 intPairs) >= 0
test_2_6 = assertEqual True $ fromJust (elemIndex x6 intPairs) >= 0

xs7 = [1,3..]
ys7 = [2,4..]
ans7 = 2*10^7
test_2_7 = assertEqual True $ fromJust (elemIndex ans7 $ Interleave.interleave xs7 ys7) >= 0
test_2_8 = assertEqual True $ fromJust (elemIndex ans7 $ Interleave.interleave ys7 xs7) >= 0

test_3_1 = assertEqual 6 (MaxSubstrSum.solution [4,-5,1,2,3,0,-2,1])
test_3_2 = assertEqual 0 (MaxSubstrSum.solution [-5])