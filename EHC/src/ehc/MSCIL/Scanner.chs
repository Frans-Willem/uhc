%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[1 module {%{EH}MSCIL.Scanner} import(Data.Char, Control.Applicative, Data.Maybe, qualified Data.Set as Set, Data.List, UU.Parsing.Interface)
%%]

%%[(8 core) export(CILToken(..), scan)
data CILToken
  = T_EOF
  | T_QSTRING String
  | T_SQSTRING String
  | T_Keyword String
  | T_Int32 Int
  | T_ID String
  | T_Punc String
  deriving (Show)

instance Eq CILToken where
  (==) T_EOF T_EOF = True
  (==) (T_QSTRING _) (T_QSTRING _) = True
  (==) (T_SQSTRING _) (T_SQSTRING _) = True
  (==) (T_Keyword a) (T_Keyword b) = (a == b)
  (==) (T_Int32 _) (T_Int32 _) = True
  (==) (T_ID _) (T_ID _) = True
  (==) (T_Punc a) (T_Punc b) = (a == b)
  (==) _ _ = False

instance Ord CILToken where
  compare T_EOF T_EOF = EQ
  compare T_EOF _ = LT
  compare _ T_EOF = LT
  compare (T_QSTRING _) (T_QSTRING _) = EQ
  compare (T_QSTRING _) _ = LT
  compare _ (T_QSTRING _) = GT
  compare (T_SQSTRING _) (T_SQSTRING _) = EQ
  compare (T_SQSTRING _) _ = LT
  compare _ (T_SQSTRING _) = GT
  compare (T_Keyword a) (T_Keyword b) = compare a b
  compare (T_Keyword _) _ = LT
  compare _ (T_Keyword _) = GT
  compare (T_Int32 _) (T_Int32 _) = EQ
  compare (T_Int32 _) _ = LT
  compare _ (T_Int32 _) = GT
  compare (T_ID _) (T_ID _) = EQ
  compare (T_ID _) _ = LT
  compare _ (T_ID _) = GT
  compare (T_Punc a) (T_Punc b) = compare a b
  compare (T_Punc _) _ = LT
  compare _ (T_Punc _) = GT

instance (Symbol CILToken) where

data ScannerError
  = UnterminatedStringLiteral
  | InvalidEscapeSequence
  | InvalidInteger
  | UnexpectedInput
  deriving (Show)

isWhite :: Char -> Bool
isWhite ' ' = True
isWhite '\t' = True
isWhite '\r' = True
isWhite '\n' = True
isWhite _ = False

isKeywordChar :: Char -> Bool
isKeywordChar n = (isAlphaNum n) || (n == '.')
isKeywordFirstChar :: Char -> Bool
isKeywordFirstChar n = (isKeywordChar n) || (n == '#')


keywords :: Set.Set String
keywords = Set.fromList
  [ "#line"
  , ".addon"
  , ".assembly"
  , ".cctor"
  , ".class"
  , ".corflags"
  , ".ctor"
  , ".custom"
  , ".data"
  , ".emitbyte"
  , ".entrypoint"
  , ".event"
  , ".export"
  , ".field"
  , ".file"
  , ".fire"
  , ".get"
  , ".hash"
  , ".imagebase"
  , ".import"
  , ".language"
  , ".line"
  , ".locale"
  , ".localized"
  , ".locals"
  , ".manifestres "
  , ".maxstack"
  , ".method"
  , ".module"
  , ".mresource"
  , ".namespace"
  , ".other"
  , ".override"
  , ".pack"
  , ".param"
  , ".pdirect"
  , ".permission"
  , ".permissionset"
  , ".property"
  , ".publickey"
  , ".publickeytoken"
  , ".removeon"
  , ".set"
  , ".size"
  , ".subsystem"
  , ".try"
  , ".ver"
  , ".vtable"
  , ".vtentry"
  , ".vtfixup"
  , ".zeroinit"
  , "^THE_END^"
  , "abstract"
  , "add"
  , "add.ovf"
  , "add.ovf.un"
  , "algorithm"
  , "alignment"
  , "and"
  , "ansi"
  , "any"
  , "arglist"
  , "array"
  , "as "
  , "assembly"
  , "assert"
  , "at"
  , "auto"
  , "autochar"
  , "beforefieldinit"
  , "beq"
  , "beq.s"
  , "bge"
  , "bge.s"
  , "bge.un"
  , "bge.un.s"
  , "bgt"
  , "bgt.s"
  , "bgt.un"
  , "bgt.un.s"
  , "ble"
  , "ble.s"
  , "ble.un"
  , "ble.un.s"
  , "blob"
  , "blob_object"
  , "blt"
  , "blt.s"
  , "blt.un"
  , "blt.un.s"
  , "bne.un"
  , "bne.un.s"
  , "bool"
  , "box"
  , "br"
  , "br.s"
  , "break"
  , "brfalse"
  , "brfalse.s"
  , "brinst"
  , "brinst.s"
  , "brnull "
  , "brnull.s"
  , "brtrue"
  , "brtrue.s"
  , "brzero"
  , "brzero.s"
  , "bstr"
  , "bytearray"
  , "byvalstr"
  , "call"
  , "calli"
  , "callmostderived"
  , "callvirt"
  , "carray"
  , "castclass"
  , "catch"
  , "cdecl"
  , "ceq"
  , "cf"
  , "cgt"
  , "cgt.un"
  , "char"
  , "cil"
  , "ckfinite"
  , "class"
  , "clsid"
  , "clt"
  , "clt.un"
  , "const"
  , "constrained."
  , "conv.i"
  , "conv.i1"
  , "conv.i2"
  , "conv.i4"
  , "conv.i8"
  , "conv.ovf.i"
  , "conv.ovf.i.un"
  , "conv.ovf.i1"
  , "conv.ovf.i1.un "
  , "conv.ovf.i2"
  , "conv.ovf.i2.un"
  , "conv.ovf.i4"
  , "conv.ovf.i4.un"
  , "conv.ovf.i8"
  , "conv.ovf.i8.un"
  , "conv.ovf.u"
  , "conv.ovf.u.un"
  , "conv.ovf.u1"
  , "conv.ovf.u1.un"
  , "conv.ovf.u2"
  , "conv.ovf.u2.un"
  , "conv.ovf.u4"
  , "conv.ovf.u4.un"
  , "conv.ovf.u8"
  , "conv.ovf.u8.un"
  , "conv.r.un"
  , "conv.r4"
  , "conv.r8"
  , "conv.u"
  , "conv.u1"
  , "conv.u2"
  , "conv.u4"
  , "conv.u8"
  , "cpblk"
  , "cpobj"
  , "currency"
  , "custom"
  , "date"
  , "decimal"
  , "default"
  , "default"
  , "demand"
  , "deny"
  , "div"
  , "div.un"
  , "dup"
  , "endfault "
  , "endfilter"
  , "endfinally"
  , "endmac"
  , "enum"
  , "error"
  , "explicit"
  , "extends"
  , "extern"
  , "false"
  , "famandassem"
  , "family"
  , "famorassem"
  , "fastcall"
  , "fastcall"
  , "fault"
  , "field"
  , "filetime"
  , "filter"
  , "final"
  , "finally"
  , "fixed"
  , "float"
  , "float32"
  , "float64"
  , "forwardref"
  , "fromunmanaged"
  , "handler"
  , "hidebysig"
  , "hresult"
  , "idispatch"
  , "il"
  , "illegal"
  , "implements"
  , "implicitcom"
  , "implicitres"
  , "import"
  , "in"
  , "inheritcheck"
  , "init"
  , "initblk"
  , "initobj"
  , "initonly"
  , "instance"
  , "int"
  , "int16"
  , "int32"
  , "int64"
  , "int8"
  , "interface"
  , "internalcall"
  , "isinst"
  , "iunknown"
  , "jmp"
  , "lasterr"
  , "lcid"
  , "ldarg"
  , "ldarg.0"
  , "ldarg.1"
  , "ldarg.2"
  , "ldarg.3"
  , "ldarg.s"
  , "ldarga"
  , "ldarga.s"
  , "ldc.i4"
  , "ldc.i4.0"
  , "ldc.i4.1"
  , "ldc.i4.2"
  , "ldc.i4.3"
  , "ldc.i4.4"
  , "ldc.i4.5"
  , "ldc.i4.6"
  , "ldc.i4.7"
  , "ldc.i4.8"
  , "ldc.i4.M1"
  , "ldc.i4.m1"
  , "ldc.i4.s "
  , "ldc.i8"
  , "ldc.r4"
  , "ldc.r8"
  , "ldelem"
  , "ldelem.i"
  , "ldelem.i1"
  , "ldelem.i2"
  , "ldelem.i4"
  , "ldelem.i8"
  , "ldelem.r4"
  , "ldelem.r8"
  , "ldelem.ref"
  , "ldelem.u1"
  , "ldelem.u2"
  , "ldelem.u4"
  , "ldelem.u8"
  , "ldelema"
  , "ldfld"
  , "ldflda"
  , "ldftn"
  , "ldind.i"
  , "ldind.i1"
  , "ldind.i2"
  , "ldind.i4"
  , "ldind.i8"
  , "ldind.r4"
  , "ldind.r8"
  , "ldind.ref"
  , "ldind.u1"
  , "ldind.u2"
  , "ldind.u4"
  , "ldind.u8"
  , "ldlen"
  , "ldloc"
  , "ldloc.0"
  , "ldloc.1"
  , "ldloc.2"
  , "ldloc.3 "
  , "ldloc.s"
  , "ldloca"
  , "ldloca.s"
  , "ldnull"
  , "ldobj"
  , "ldsfld"
  , "ldsflda"
  , "ldstr"
  , "ldtoken"
  , "ldvirtftn"
  , "leave"
  , "leave.s"
  , "linkcheck"
  , "literal"
  , "localloc"
  , "lpstr"
  , "lpstruct"
  , "lptstr"
  , "lpvoid"
  , "lpwstr"
  , "managed"
  , "marshal"
  , "method"
  , "mkrefany"
  , "modopt"
  , "modreq"
  , "mul"
  , "mul.ovf"
  , "mul.ovf.un"
  , "native"
  , "neg"
  , "nested"
  , "newarr"
  , "newobj"
  , "newslot"
  , "noappdomain"
  , "no."
  , "noinlining "
  , "nomachine"
  , "nomangle"
  , "nometadata"
  , "noncasdemand"
  , "noncasinheritance"
  , "noncaslinkdemand"
  , "nop"
  , "noprocess"
  , "not"
  , "not_in_gc_heap"
  , "notremotable"
  , "notserialized"
  , "null"
  , "nullref"
  , "object"
  , "objectref"
  , "opt"
  , "optil"
  , "or"
  , "out"
  , "permitonly"
  , "pinned"
  , "pinvokeimpl"
  , "pop"
  , "prefix1"
  , "prefix2"
  , "prefix3"
  , "prefix4"
  , "prefix5"
  , "prefix6"
  , "prefix7"
  , "prefixref"
  , "prejitdeny"
  , "prejitgrant"
  , "preservesig"
  , "private"
  , "privatescope"
  , "protected "
  , "public"
  , "readonly."
  , "record"
  , "refany"
  , "refanytype"
  , "refanyval"
  , "rem"
  , "rem.un"
  , "reqmin"
  , "reqopt"
  , "reqrefuse"
  , "reqsecobj"
  , "request"
  , "ret"
  , "rethrow"
  , "retval"
  , "rtspecialname"
  , "runtime"
  , "safearray"
  , "sealed"
  , "sequential"
  , "serializable"
  , "shl"
  , "shr"
  , "shr.un"
  , "sizeof"
  , "special"
  , "specialname"
  , "starg"
  , "starg.s"
  , "static"
  , "stdcall"
  , "stdcall"
  , "stelem"
  , "stelem.i"
  , "stelem.i1"
  , "stelem.i2"
  , "stelem.i4 "
  , "stelem.i8"
  , "stelem.r4"
  , "stelem.r8"
  , "stelem.ref"
  , "stfld"
  , "stind.i"
  , "stind.i1"
  , "stind.i2"
  , "stind.i4"
  , "stind.i8"
  , "stind.r4"
  , "stind.r8"
  , "stind.ref"
  , "stloc"
  , "stloc.0"
  , "stloc.1"
  , "stloc.2"
  , "stloc.3"
  , "stloc.s"
  , "stobj"
  , "storage"
  , "stored_object"
  , "stream"
  , "streamed_object"
  , "string"
  , "struct"
  , "stsfld"
  , "sub"
  , "sub.ovf"
  , "sub.ovf.un"
  , "switch"
  , "synchronized"
  , "syschar"
  , "sysstring"
  , "tail."
  , "tbstr"
  , "thiscall"
  , "thiscall "
  , "throw"
  , "tls"
  , "to"
  , "true"
  , "typedref"
  , "unaligned."
  , "unbox"
  , "unbox.any"
  , "unicode"
  , "unmanaged"
  , "unmanagedexp"
  , "unsigned"
  , "unused"
  , "userdefined"
  , "value"
  , "valuetype"
  , "vararg"
  , "variant"
  , "vector"
  , "virtual"
  , "void"
  , "volatile."
  , "wchar"
  , "winapi"
  , "with"
  , "wrapper"
  , "xor"
  ]

punctuation :: [String]
punctuation =
  [ "{"
  , "}"
  , "["
  , "]"
  , "("
  , ")"
  , ","
  , "::"
  , ":"
  , ";"
  , "="
  , "*"
  , "&"
  , "+"
  , "/"
  , "!!"
  , "!"
  , "<"
  , ">"
  , "..."
  , "."
  ]

isValidIdStartChar :: Char -> Bool
isValidIdStartChar n = (isAlpha n) || (n=='_') || (n=='$') || (n=='@') || (n=='`') || (n=='?')

isValidIdChar :: Char -> Bool
isValidIdChar n = (isValidIdStartChar n) || (isDigit n)

scan :: String -> Int -> Either (Int,ScannerError) [CILToken]
scan inp pos =
  do (token, rest) <- popToken inp pos
     if (token == T_EOF)
       then return []
       else do restTokens <- scan rest (pos + (length rest))
               return (token:restTokens)

popToken :: String -> Int -> Either (Int, ScannerError) (CILToken, String)
popToken [] _ = Right (T_EOF, "")
-- Double and single quoted strings
popToken ('"':rest) pos
  = fmap (\(x,y) -> (T_QSTRING x, y)) $ popStringToken '"' rest (pos+1)
popToken ('\'':rest) pos
  = fmap (\(x,y) -> (T_SQSTRING x, y)) $ popStringToken '\'' rest (pos+1)
-- Anything else
popToken input pos
  = let (white, nonwhite) = span isSpace input
    in if (null white)
         then
           fromJust
             ( popIntToken input pos
             <|> popKeywordToken input pos
             <|> popPunctuationToken input pos
             <|> popIdToken input pos
             <|> (Just $ Left (pos, UnexpectedInput))
             )
         else popToken nonwhite (pos + (length white))

{- Try to pop a punctuation token -}
popPunctuationToken :: String -> Int -> Maybe (Either (Int, ScannerError) (CILToken, String))
popPunctuationToken input pos
  = foldr (<|>) Nothing $ map (\p -> if p `isPrefixOf` input then (Just $ Right (T_Punc p, drop (length p) input)) else Nothing) punctuation

{- Try to pop an integer token -}
popIntToken :: String -> Int -> Maybe (Either (Int, ScannerError) (CILToken, String))
popIntToken ('0':'x':xs) pos 
  = let (digits, nondigits) = span isHexDigit xs
    in if (length digits == 0)
         then Just $ Left (pos, InvalidInteger)
         else Just $ Right (T_Int32 $ foldl (\x y -> (x*16)+y) 0 (map digitToInt digits), nondigits)
popIntToken input pos
  = let (digits, nondigits) = span isDigit input
    in if null digits
         then Nothing
         else Just $ Right (T_Int32 $ foldl (\x y -> (x*10)+y) 0 (map digitToInt digits), nondigits)

{- Try to pop a keyword -}
popKeywordToken :: String -> Int -> Maybe (Either (Int, ScannerError) (CILToken, String))
popKeywordToken (x:xs) pos
  = if (not (isKeywordFirstChar x))
      then Nothing
      else
        let
          (keywordrest, rest) = span isKeywordChar xs
          keyword = (x:keywordrest)
        in
          if (Set.notMember keyword keywords)
            then Nothing
            else Just $ Right (T_Keyword keyword, rest)

{- Try to pop an Id token -}
popIdToken :: String -> Int -> Maybe (Either (Int, ScannerError) (CILToken, String))
popIdToken xxs@(x:xs) pos
  = if (not (isValidIdStartChar x))
      then Nothing
      else
        let (id, rest) = span isValidIdChar xxs
        in Just $ Right (T_ID id, rest)

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (a,b) = (f a, b)

{- QSTRING and SQSTRING -}
popStringToken :: Char -> String -> Int -> Either (Int, ScannerError) (String, String)
popStringToken _ [] pos
  = Left (pos,UnterminatedStringLiteral)
-- Basic escaped characters
popStringToken c ('\\':'n':rest) pos
  = fmap (mapFst ('\n':)) $ popStringToken c rest (pos+2)
popStringToken c ('\\':'t':rest) pos
  = fmap (mapFst ('\t':)) $ popStringToken c rest (pos+2)
-- Escaped newlines
popStringToken c ('\\':'\n':rest) pos
  = popStringToken c nonwhite (pos + 2 + (length white))
  where (white, nonwhite) = span isWhite rest
popStringToken c ('\\':'\r':'\n':rest) pos
  = popStringToken c nonwhite (pos + 3 + (length white))
  where (white, nonwhite) = span isWhite rest
-- Escaped octal sequence
popStringToken c ('\\':rest) pos
  = if (length rest < 3)
      then Left (pos, InvalidEscapeSequence)
      else
        let (escaped, after) = splitAt 3 rest
        in if (not $ all isOctDigit escaped)
             then Left (pos, InvalidEscapeSequence)
             else fmap (mapFst ((escapeOct escaped):)) $ popStringToken c after (pos + 4)
  where
    escapeOct :: [Char] -> Char
    escapeOct l = chr (foldl (\x y -> (x*8)+y) 0 (map digitToInt l))
popStringToken c (x:xs) pos =
  if (c == x)
    then Right ("",xs)
    else fmap (mapFst (x:)) $ popStringToken c xs (pos + 1)
%%]
