{- ----------------------------------------------------------------------------------------
   what    : test for previous bug, loop in EH.Ty.tyAppFunArgsWithLkup
             test for correct resolution of overloading of a, and indirectly also c,
                  however as of 20090819 not resolved yet (pun intended)
                  as of 20100918: correct
   expected: correct resolution for c
---------------------------------------------------------------------------------------- -}

module AmbigType1 where

main = print t

t :: Int
t = let a = id [ 1, 2]
        b@(c:_) = a -- (a :: [Int])
    in  c -- b
