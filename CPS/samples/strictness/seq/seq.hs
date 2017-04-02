foreign import prim "dup; call default void class [corlib]System.Console::WriteLine(object)" echoEval :: a -> a

seq a b = let !c = a in b

func a b = a `seq` b

main = func (echoEval 10) 20
