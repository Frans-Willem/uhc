data CLRInt32
data CLRVoid
data [] a = ''[]'' | a : [a]
data Bool = False | True
data Ordering = LT | EQ | GT

{-
foreign import ccall "primCStringToString" packedStringToString :: PackedString -> [Char]
foreign import ccall "primTraceStringExit" traceStringExit :: [Char] -> [Char]
-}

foreign import prim "conv.i .override native int(int32)" primInt32ToInt :: CLRInt32 -> Int
foreign import prim "conv.i4 .override int32(native int)" primIntToInt32 :: Int -> CLRInt32
foreign import prim "add" (+) :: Int -> Int -> Int
foreign import prim "div" (/) :: Int -> Int -> Int
foreign import prim "mul" (*) :: Int -> Int -> Int
foreign import prim "sub" (-) :: Int -> Int -> Int
foreign import prim "rem" rem :: Int -> Int -> Int
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

filter p (h:t) = if p h then h : filter p t else filter p t
filter _ []    = []

take 0 _ = []
take _ []    = []
take n (h:t) = h : (take (n-1) t)

length (h:t) = 1 + length t
length []    = 0

sum [] = 0
sum (h:t) = h + sum t


not True = False
not False = True

-- Return true if y is not a multiple of x
notMultiple x y = not ((y `rem` x) == 0)

sieve (h:t) = h : sieve (filter (notMultiple h) t)

main = sum (take 20 (sieve [2..]))

enumFrom s = s : enumFrom (s+1)
