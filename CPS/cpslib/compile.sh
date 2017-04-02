mcs -target:library cpslib.cs
mcs -r:`pkg-config --variable=Libraries cecil` TailCall.cs
mono TailCall.exe cpslib.dll
