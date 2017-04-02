foreign import prim "dup; call default void class [corlib]System.Console::WriteLine(object)" echoEval :: a -> a

f1 x = 10
main = f1 (echoEval 123)
