foreign import prim "add" (+) :: Int -> Int -> Int

data Bool = True | False
(==) :: Int -> Int -> Bool

x = 10

y =
  case x of
    1 -> 1
    10 -> 2
    100 -> 3
    _ -> 99

z =
  case x of
    1 -> 0
    2 -> 1
    3 -> 2
    _ -> 99

main = y + z
