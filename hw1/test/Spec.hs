import qualified APlusB(solution)
import qualified LCM(solution)
import qualified Distance(solution)
main :: IO ()
main = do
    print $ APlusB.solution 2 2 == 4
    print $ APlusB.solution (-1) 2 == 1
    print $ LCM.solution 6 4 == 12
    print $ Distance.solution 3 (1,2) (2,4) == 2
