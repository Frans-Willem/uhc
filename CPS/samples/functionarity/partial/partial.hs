foreign import prim "add" (+) :: Int -> Int -> Int

f a b c = a + b + c

f1 = f 1

addrest x = x 2 3

main = addrest f1
