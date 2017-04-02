data Blah
foreign import prim "add" (+) :: Int -> Int -> Int

a :: Int
a = 13

b :: Int
b = 37

main = a + b
