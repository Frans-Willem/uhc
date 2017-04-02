foreign import prim "ldstr \"Eval\"; call default void class [corlib]System.Console::WriteLine(string) .override object(string, object)" debugEval :: a -> a
foreign import prim "add" (+) :: Int -> Int -> Int

shared = debugEval (1 + 2)

x = shared
y = shared
z = x + y

main = z
