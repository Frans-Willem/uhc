foreign import prim "add" (+) :: Int -> Int -> Int
foreign import prim "sub" (-) :: Int -> Int -> Int

(==) :: Int -> Int -> Int

fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

main = fib 5
