foreign import prim "dup; call default void class [corlib]System.Console::WriteLine(object)" echoEval :: a -> a

data Hello = World !Int Int Int

f1 (World a b c) = 20

main = f1 (World (echoEval 1) (echoEval 2) (echoEval 3))
