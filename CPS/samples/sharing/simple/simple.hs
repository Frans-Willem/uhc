foreign import prim "dup; call default void class [corlib]System.Console::WriteLine(object)" evalDump :: a -> a
foreign import prim "add" (+) :: Int -> Int -> Int

shared = (evalDump (2 + 3))

main = shared + shared
