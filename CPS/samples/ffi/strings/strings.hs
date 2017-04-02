-- Basic data types
data [] a = ''[]'' | a : [a]
data Bool = True | False

-- CLR types
data CLRString -- string
data CLRInt32 -- int32
data CLRCharArr -- char[]
data CLRVoid -- void, only value null

-- Foreign imports
foreign import prim "newarr char" primNewCharArr :: Int -> CLRCharArr
foreign import prim "stelem char" primUpdCharArr :: CLRCharArr -> Int -> Char -> CLRVoid

foreign import prim "'' .override object(object '%1')" primSeq :: a -> b -> b

foreign import prim "newobj instance void string::.ctor(char[])" primNewString :: CLRCharArr -> CLRString

foreign import prim "callvirt instance int32 string::get_Length()" primStringGetLength :: CLRString -> CLRInt32
foreign import prim "callvirt instance char string::get_Chars(int32)" primStringGetChar :: CLRString -> CLRInt32 -> Char

foreign import prim "conv.i .override native int(int32)" primInt32ToInt :: CLRInt32 -> Int
foreign import prim "conv.i4 .override int32(native int)" primIntToInt32 :: Int -> CLRInt32

foreign import prim "add" primAdd :: Int -> Int -> Int
foreign import prim "sub" primSub :: Int -> Int -> Int
foreign import prim "ceq" primCeq :: Int -> Int -> CLRInt32
foreign import prim "clt" primClt :: Int -> Int -> CLRInt32
foreign import prim "cgt" primCgt :: Int -> Int -> CLRInt32

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

(+) :: Int -> Int -> Int
(+) = primAdd

(-) :: Int -> Int -> Int
(-) = primSub

primListToArray :: (Int -> a) -> (a -> Int -> b -> c) -> [b] -> a
primListToArray newarr updarr lst
  = foldl (\arr (idx,elm) -> (updarr arr idx elm) `primSeq` arr) (newarr l) (zip [0..] lst)
  where
    l = length lst

-- Basic prelude-like functions
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + (length xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (a:as) (b:bs) = (a,b):(zip as bs)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = (f x):(map f xs)

enumFrom :: Int -> [Int]
enumFrom n = n : (enumFrom (n `primAdd` 1))

enumFromTo :: Int -> Int -> [Int]
enumFromTo n m
  = case (n > m) of
      True -> []
      False -> n : (enumFromTo (n `primAdd` 1) m)

(++) :: [a] -> [a] -> [a]
(++) [] bs = bs
(++) (a:as) bs = a : (as ++ bs)

-- String helpers
packedStringToString :: CLRString -> [Char]
packedStringToString clrstr
  = map getc [0..(l - 1)]
  where
    l = primInt32ToInt (primStringGetLength clrstr)
    getc idx = primStringGetChar clrstr (primIntToInt32 idx)

stringToPackedString :: [Char] -> CLRString
stringToPackedString str
  = primNewString (primListToArray primNewCharArr primUpdCharArr str)

main = stringToPackedString ("Hello" ++ " " ++ "World")
