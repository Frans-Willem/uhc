data CLRInt32
data [] a = ''[]'' | a : [a]
data Bool = False | True


foreign import prim "conv.i .override native int(int32)" primInt32ToInt :: CLRInt32 -> Int
foreign import prim "conv.i4 .override int32(native int)" primIntToInt32 :: Int -> CLRInt32
foreign import prim "add" (+) :: Int -> Int -> Int
foreign import prim "sub" (-) :: Int -> Int -> Int
foreign import prim "ceq" primCeq :: Int -> Int -> CLRInt32
foreign import prim "clt" primClt :: Int -> Int -> CLRInt32
foreign import prim "cgt" primCgt :: Int -> Int -> CLRInt32
--
-- Primitive wrappers
primCompareToBool :: (a -> a -> CLRInt32) -> (a -> a -> Bool)
primCompareToBool cmp a b
  = case (primInt32ToInt (cmp a b)) of
      0 -> False
      1 -> True

(<) :: Int -> Int -> Bool
(<) = primCompareToBool primClt

(>) :: Int -> Int -> Bool
(>) = primCompareToBool primCgt

(==) :: Int -> Int -> Bool
(==) = primCompareToBool primCeq

take 0 _ = []
take _ []    = []
take n (h:t) = h : (take (n-1) t)

sum [] = 0
sum (h:t) = h + sum t

-- See: https://www.haskell.org/tutorial/patterns.html 

-- Client starts with init, then always requests previous response + 1
init = 1
next response = response + 1
client init ~(resp:resps) = init : client (next resp) resps

-- Server always responds with request + 3
process req = req + 3
server (req:reqs) = process req : server reqs


reqs = client init resps
resps = server reqs

main  = sum (take 5 resps)
