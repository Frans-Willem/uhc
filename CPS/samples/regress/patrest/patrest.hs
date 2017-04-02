foreign import prim "'' .override object(object)" preventInline :: a -> a
foreign import prim "call void class [mscorlib]System.Console::WriteLine(object) .override object(object '%0', object '%0')" printReturn :: a -> a

data SomeType = SomeCons Int


main =
	case (preventInline (SomeCons 3)) of
		rest@(SomeCons n) -> printReturn rest
