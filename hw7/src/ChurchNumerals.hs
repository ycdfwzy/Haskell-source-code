module ChurchNumerals where

c0 f x = x
-- Be careful if you want to write type signatures for these operations
cSucc cn f x = f $ cn f x

cToInt cn = cn (+1) 0

cPlus cn cm= \f -> \x -> cm f (cn f x)

cMult cn cm = cn . cm

cExp cn cm = cm $ cn