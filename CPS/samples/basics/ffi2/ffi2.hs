data [] a = ''[]'' | a : [a]
data CLRString
data CLRInt32

-- prim should support call, calli, callvirt, castclass, 
-- Parameter names can be %0, %1, ... %n to indicate which argument to pass.
-- When parameter name does not indicate which argument to pass, it will take the same as the index in the parameter list.
foreign import prim "call default void class ['corlib']System.Console::WriteLine(string, object)" primWriteLine :: CLRString -> a -> ()

data CLRCharArray
foreign import prim "'newarr char' .override char[](native int)" primCreateCharArray :: Int -> CLRCharArray
foreign import prim "'stelem char' .override char[](char[] '%0', char[] '%0', native int '%1', char '%2')" primUpdateCharArray :: CLRCharArray -> Int -> Char -> CLRCharArray
foreign import prim "newobj instance void string::.ctor(char[])" primCreateString :: CLRCharArray -> CLRString

-- Parameters names can also be %0:name to keep the name part in the output intermediate language
-- with an instance call, the first argument will always be passed as 'this', and unnamed parameters will take the arguments behind those.
foreign import prim "callvirt instance int32 string::get_Length()" primStringGetLength :: CLRString -> CLRInt32
foreign import prim "callvirt instance char string::get_Chars(int32)" primStringGetChar :: CLRString -> CLRInt32 -> Char
foreign import prim "'' .override native int(int32)" primFromInt32 :: CLRInt32 -> Int
foreign import prim "'' .override int32(native int)" primToInt32 :: Int -> CLRInt32


-- Most raw instructions are supported and have expected parameters and types
--foreign import prim "ldstr \"Hello world\"" primHelloWorld :: CLRString
foreign import prim "add"  primAdd :: Int -> Int -> Int
foreign import prim "sub"  primSub :: Int -> Int -> Int
-- Although you may want to override the default, for example to annotate parameter stack locations
foreign import prim "pop .override object(object '%1', object '%0')" primSeq :: a -> b -> b
-- To use instructions that are unsupported by the parser/AST, simply use an id like such
foreign import prim "'div' .override native int(native int, native int)" primDiv :: Int -> Int -> Int
-- Individual instructions may have multiple return values, so brackets can be used.
-- For no return values, either void or () is acceptable.
-- However, note that the final type must be a singular type
data CLRIntTuple
foreign import prim "'dup' .override (native int, native int)(object '%0'); newobj instance void class ['corlib']Tuple<native int, native int>::.ctor(native int, native int)" primSillyTuples :: Int -> CLRIntTuple

-- Cheating a bit, maybe this should be ccall?
foreign import prim "ldstr \"Output: {0}\"" primStrOutput :: CLRString

-- Bool & helpers
data Bool = True | False
data CLRBool
foreign import prim "ldc.i4 0 .override bool()" primBoolFalse :: CLRBool
foreign import prim "ldc.i4 1 .override bool()" primBoolTrue :: CLRBool
foreign import prim "ceq .override native int(bool, bool)" primBoolEq :: CLRBool -> CLRBool -> Int

clrbool2Bool :: CLRBool -> Bool
clrbool2Bool i
  = case (primBoolEq i primBoolFalse) of
      0 -> True
      1 -> False

bool2CLRBool :: Bool -> CLRBool
bool2CLRBool True = primBoolTrue
bool2CLRBool False = primBoolFalse

-- Int helpers
foreign import prim "ceq .override native int(native int, native int)" primIntEq :: Int -> Int -> Int
foreign import prim "clt .override native int(native int, native int)" primIntLt :: Int -> Int -> Int
foreign import prim "cgt .override native int(native int, native int)" primIntGt :: Int -> Int -> Int



error :: [Char] -> a

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 `primAdd` (length xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (a:as) (b:bs) = (a,b):(zip as bs)

enumFrom :: Int -> [Int]
enumFrom n = n : (enumFrom (n `primAdd` 1))

enumFromTo :: Int -> Int -> [Int]
enumFromTo n m
  = case (n `primIntGt` m) of
      1 -> []
      0 -> n : (enumFromTo (n `primAdd` 1) m)

(==) :: Int -> Int -> Bool
(==) a b
  = case (a `primIntEq` b) of
      1 -> True
      0 -> False

stringToCLRString :: [Char] -> CLRString
stringToCLRString str
  = primCreateString finalArr
  where
    l = length str
    initArr = primCreateCharArray l
    update arr (idx, c) = primUpdateCharArray arr idx c
    finalArr = foldl update initArr (zip [0..] str)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x):(map f xs)

output :: a -> a
output a = (primWriteLine primStrOutput a) `primSeq` a

packedStringToString :: CLRString -> [Char]
packedStringToString clrstr
  = map getc [0..(l `primSub` 1)]
  where
    l = primFromInt32 (primStringGetLength clrstr)
    getc idx = primStringGetChar clrstr (primToInt32 idx)

(++) :: [a] -> [a] -> [a]
(++) [] bs = bs
(++) (a:as) bs = a : (as ++ bs)

main = stringToCLRString ("Hello world" ++ "Poepchinees")
