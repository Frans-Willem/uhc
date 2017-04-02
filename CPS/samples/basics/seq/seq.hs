data IO a

foreign import prim "call void class [mscorlib]System.Console::WriteLine(object)" writeLine :: a -> ()
foreign import prim "pop .override object(object '%1',object '%0')" seq :: a -> b -> a

main =
    (writeLine 1) `seq`
    (writeLine 2) `seq`
	(writeLine 3)
