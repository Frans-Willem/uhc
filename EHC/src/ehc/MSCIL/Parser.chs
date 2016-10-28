%%[0 hs
{-# LANGUAGE MagicHash, UnboxedTuples #-}
%%]
%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[1 module {%{EH}MSCIL.Parser} import(UU.Parsing, UHC.Util.ParseUtils, {%{EH}MSCIL}, Data.Char, {%{EH}MSCIL.Scanner}, Data.Bifunctor)
%%]

%%[(8 core) export(pType, pId, pDottedName, pParam, pParamL, parseString, pPunc, pKeyword, CILParser(..), pInstruction, pSQSTRING, pTypeSpec)
data CILInput = CILInput [CILToken] !Int
type CILParser a = AnaParser CILInput Pair CILToken Int a

instance InputState CILInput CILToken Int where
  splitStateE (CILInput [] pos) = Right' (CILInput [] pos)
  splitStateE (CILInput (T_EOF:_) pos) = Left' T_EOF (CILInput [] (pos+1))
  splitStateE (CILInput (x:xs) pos) = Left' x (CILInput xs (pos+1))
  splitState (CILInput (x:xs) pos) = (# x, CILInput xs (pos+1) #)
  getPosition (CILInput _ pos) = pos

parseString :: CILParser a -> String -> Either String a
parseString p str =
  do tokens <- first (show) $ scan str 0
     let parseResult = parse p (CILInput tokens 0)
         messages = getMsgs parseResult
         (Pair result _) = evalSteps parseResult
     if null messages
       then return result
       else fail $ show messages

{- Base parsers -}
pEOF :: CILParser ()
pEOF = () <$ pSym (T_EOF)

pQSTRING :: CILParser String
pQSTRING
  = extract <$> pSym (T_QSTRING "")
  where extract (T_QSTRING x) = x

pSQSTRING :: CILParser String
pSQSTRING
  = extract <$> pSym (T_SQSTRING "")
  where extract (T_SQSTRING x) = x

pKeyword :: String -> CILParser ()
pKeyword k = () <$ pSym (T_Keyword k)

pInteger :: CILParser Int
pInteger
  = extract <$> pSym (T_Int32 0)
  where extract (T_Int32 n) = n

pID :: CILParser String
pID
  = extract <$> pSym (T_ID "")
  where extract (T_ID x) = x

pPunc :: String -> CILParser ()
pPunc p = () <$ pSym (T_Punc p)

-- Derived parsers
pId :: CILParser Id
pId = Id_Id <$> (pID <|> pSQSTRING)

pDottedName :: CILParser DottedName
pDottedName = pList1Sep (pPunc ".") pId

pSimpleType :: CILParser Type
pSimpleType
  = pAny (\(ty, k) -> ty <$ pKeyword k)
      [ (Type_Bool, "bool")
      , (Type_Char, "char")
      , (Type_Float32, "float32")
      , (Type_Float64, "float64")
      , (Type_Int8, "int8")
      , (Type_Int16, "int16")
      , (Type_Int32, "int32")
      , (Type_Int64, "int64")
      , (Type_Object, "object")
      , (Type_String, "string")
      , (Type_TypedRef, "typedref")
      , (Type_Void, "void")
      ]
  <|> (Type_GenMethodParam <$> (pPunc "!!" *> pInteger))
  <|> (Type_GenTypeParam <$> (pPunc "!" *> pInteger))
  <|> Type_Class <$ pKeyword "class" <*> pTypeReference
  <|> Type_Method <$ pKeyword "method" <*> pCallConv <*> pType <* pPunc "*" <*> pParamL
  <|> pKeyword "native" *>
    ( Type_NativeInt <$ pKeyword "int"
    <|> Type_NativeUInt <$ pKeyword "unsigned" <* pKeyword "int"
    )
  <|> Type_Valuetype <$ pKeyword "valuetype" <*> pTypeReference
  <|> pKeyword "unsigned" *>
    ( Type_UInt8 <$ pKeyword "int8"
    <|> Type_UInt16 <$ pKeyword "int16"
    <|> Type_UInt32 <$ pKeyword "int32"
    <|> Type_UInt64 <$ pKeyword "int64"
    )


pBound :: CILParser Bound
pBound
  = Bound_Unspecified <$ pPunc "..."
  <|> Bound_ZeroTo <$> pInteger
  <|> Bound_From <$> pInteger <* pPunc "..."
  <|> Bound_Range <$> pInteger <* pPunc "..." <*> pInteger

pTypeModifier :: CILParser (Type -> Type)
pTypeModifier
  = Type_ManagedPtr <$ pPunc "&"
  <|> Type_UnmanagedPtr <$ pPunc "*"
  <|> (flip Type_GenType) <$> pPacked (pPunc "<") (pPunc ">") (pList1Sep (pPunc ",") pType)
  <|> (flip Type_Array) <$> pPacked (pPunc "[") (pPunc "]") (pListSep (pPunc ",") pBound)
  <|> (flip Type_ModOpt) <$ pKeyword "modopt" <*> pPacked (pPunc "(") (pPunc ")") pTypeReference
  <|> (flip Type_ModReq) <$ pKeyword "modreq" <*> pPacked (pPunc "(") (pPunc ")") pTypeReference
  <|> Type_Pinned <$ pKeyword "pinned"

pTypeModifiers :: CILParser (Type -> Type)
pTypeModifiers = pFoldr (flip (.), id) pTypeModifier

pType :: CILParser Type
pType = pSimpleType <**> pTypeModifiers

pParamAttr :: CILParser ParamAttr
pParamAttr
  = pAny (\(kw, val) -> val <$ (pPunc "[" *> pKeyword kw *> pPunc "]"))
      [ ("in", ParamAttr_In)
      , ("out", ParamAttr_Out)
      , ("opt", ParamAttr_Opt)
      ]

pParam :: CILParser Param
pParam = Param_Param <$> pList pParamAttr <*> pType <*> pMb pId

pParamL :: CILParser ParamL
pParamL = pPacked (pPunc "(") (pPunc ")") (pListSep (pPunc ",") pParam)

pMethodName :: CILParser MethodName
pMethodName =
  (MethodName_Ctor <$ pKeyword ".ctor") <|>
  (MethodName_Cctor <$ pKeyword ".cctor") <|>
  (MethodName_Name <$> pDottedName)

pFilename :: CILParser DottedName
pFilename = pDottedName

pAssemblyRefName :: CILParser AssemblyRefName
pAssemblyRefName = AssemblyRefName_AssemblyRefName <$> pDottedName

pResolutionScope :: CILParser ResolutionScope
pResolutionScope =
  pPacked (pPunc "[") (pPunc "]") (
    (ResolutionScope_Module <$> (pKeyword ".module" *> pFilename)) <|>
    (ResolutionScope_AssemblyRefName <$> pAssemblyRefName)
  )

pTypeReference :: CILParser TypeReference
pTypeReference =
  TypeReference_Ref <$>
  (pMb pResolutionScope) <*>
  pListSep (pPunc "/") pDottedName

pTypeSpec :: CILParser TypeSpec
pTypeSpec =
  (TypeSpec_External <$> pPacked (pPunc "[") (pPunc "]") (pMb (pKeyword ".module") *> pDottedName)) <|>
  (TypeSpec_Ref <$> pTypeReference) <|>
  (TypeSpec_Type <$> pType)

pCallKind :: CILParser CallKind
pCallKind
  = opt
      ( CallKind_Default <$ pKeyword "default" -- TODO: Expand
      )
      CallKind_Default

pCallConv :: CILParser CallConv
pCallConv =
  CallConv_CallConv <$>
  (opt (True <$ pKeyword "instance") False) <*>
  (opt (True <$ pKeyword "explicit") False) <*>
  pCallKind

pInstructionNoOp :: CILParser Instruction
pInstructionNoOp =
  pAny (\(i,k) -> i <$ pKeyword k)
    (
    [ (Instruction_Add, "add")
    , (Instruction_AddOvf, "add.ovf")
    , (Instruction_And, "and")
    , (Instruction_CEq, "ceq")
    , (Instruction_CGt, "cgt")
    , (Instruction_CGtUn, "cgt.un")
    , (Instruction_CLt, "clt")
    , (Instruction_CLtUn, "clt.un")
    , (Instruction_Div, "div")
    , (Instruction_DivUn, "div.un")
    , (Instruction_Dup, "dup")
    , (Instruction_LdNull, "ldnull")
    , (Instruction_Mul, "mul")
    , (Instruction_MulOvf, "mul.ovf")
    , (Instruction_MulOvfUn, "mul.ovf.un")
    , (Instruction_Neg, "neg")
    , (Instruction_Nop, "nop")
    , (Instruction_Not, "not")
    , (Instruction_Or, "or")
    , (Instruction_Pop, "pop")
    , (Instruction_Rem, "rem")
    , (Instruction_RemUn, "rem.un")
    , (Instruction_Ret, "ret")
    , (Instruction_Shl, "shl")
    , (Instruction_Shr, "shr")
    , (Instruction_ShrUn, "shr.un")
    , (Instruction_Sub, "sub")
    , (Instruction_SubOvf, "sub.ovf")
    , (Instruction_SubOvfUn, "sub.ovf.un")
    , (Instruction_Tail, "tail.")
    , (Instruction_Xor, "xor")
    ]
    ++
    (map (\i -> (Instruction_LdcI4 i, "ldc.i4." ++ (show i))) [0..8])
    ++
    [ (Instruction_LdcI4 (-1), "ldc.i4.m1")
    , (Instruction_LdcI4 (-1), "ldc.i4.M1")
    ]
    ++
    (concat $ map
      (\(ty,s) -> [(Instruction_StElem (TypeSpec_Type ty), "stelem." ++ s),(Instruction_LdElem (TypeSpec_Type ty), "ldelem." ++ s)])
      [ (Type_Int8, "i1")
      , (Type_Int16, "i2")
      , (Type_Int32, "i4")
      , (Type_Int64, "i8")
      , (Type_Float32, "r4")
      , (Type_Float64, "r8")
      , (Type_NativeInt, "i")
      ]
    )
    ++
    ( map (\(i,ty,ovf,un) -> (i, "conv." ++ (if ovf then ".ovf" else "") ++ ty ++ (if un then ".un" else "")))
      [ (Instruction_ConvI1, "i1", False, False)
      , (Instruction_ConvI2, "i2", False, False)
      , (Instruction_ConvI4, "i4", False, False)
      , (Instruction_ConvI8, "i8", False, False)
      , (Instruction_ConvR4, "r4", False, False)
      , (Instruction_ConvR8, "r8", False, False)
      , (Instruction_ConvU1, "u1", False, False)
      , (Instruction_ConvU2, "u2", False, False)
      , (Instruction_ConvU4, "u4", False, False)
      , (Instruction_ConvU8, "u8", False, False)
      , (Instruction_ConvI, "i", False, False)
      , (Instruction_ConvU, "u", False, False)
      , (Instruction_ConvRUn, "r", False, True)
      , (Instruction_ConvOvfI1, "i1", True, False)
      , (Instruction_ConvOvfI2, "i2", True, False)
      , (Instruction_ConvOvfI4, "i4", True, False)
      , (Instruction_ConvOvfI8, "i8", True, False)
      , (Instruction_ConvOvfU1, "u1", True, False)
      , (Instruction_ConvOvfU2, "u2", True, False)
      , (Instruction_ConvOvfU4, "u4", True, False)
      , (Instruction_ConvOvfU8, "u8", True, False)
      , (Instruction_ConvOvfI, "i", True, False)
      , (Instruction_ConvOvfU, "u", True, False)
      , (Instruction_ConvOvfI1Un, "i1", True, True)
      , (Instruction_ConvOvfI2Un, "i2", True, True)
      , (Instruction_ConvOvfI4Un, "i4", True, True)
      , (Instruction_ConvOvfI8Un, "i8", True, True)
      , (Instruction_ConvOvfU1Un, "u1", True, True)
      , (Instruction_ConvOvfU2Un, "u2", True, True)
      , (Instruction_ConvOvfU4Un, "u4", True, True)
      , (Instruction_ConvOvfU8Un, "u8", True, True)
      , (Instruction_ConvOvfIUn, "i", True, True)
      , (Instruction_ConvOvfUUn, "u", True, True)
      ]
    )
    )

pInstructionParamLocal :: CILParser Instruction
pInstructionParamLocal =
  (
    (Instruction_LdArg <$ pKeyword "ldarg") <|>
    (Instruction_LdArgA <$ pKeyword "ldarga") <|>
    (Instruction_LdLoc <$ pKeyword "ldloc") <|>
    (Instruction_LdLocA <$ pKeyword "ldloca") <|>
    (Instruction_StArg <$ pKeyword "starg") <|>
    (Instruction_StLoc <$ pKeyword "stloc")
  ) <*> (
    (Left <$> pInteger) <|>
    (Right <$> pId)
  )

pInstructionSingleInt :: CILParser Instruction
pInstructionSingleInt =
  (
    (Instruction_LdcI4 <$ pKeyword "ldc.i4") <|>
    (Instruction_LdcI4 <$ pKeyword "ldc.i4.s") <|>
    (Instruction_Unaligned <$ pKeyword "unaligned.") <|>
    (Instruction_LdcI8 <$ pKeyword "ldc.i8") <|>
    (Instruction_LdcI8 <$ pKeyword "ldc.i8.s")
  ) <*> pInteger

pInstructionSingleFloat :: CILParser Instruction
pInstructionSingleFloat = pFail -- TODO: Have lexer support floats

pInstructionBranch :: CILParser Instruction
pInstructionBranch =
  (
    (Instruction_Beq <$ (pKeyword "beq" <|> pKeyword "beq.s")) <|>
    (Instruction_BrFalse <$ (pKeyword "brfalse" <|> pKeyword "brfalse.s")) <|>
    (Instruction_BrTrue <$ (pKeyword "brtrue" <|> pKeyword "brtrue.s")) <|>
    (Instruction_Br <$ (pKeyword "br" <|> pKeyword "br.s"))
  ) <*> (
    (Left <$> pInteger) <|>
    (Right <$> pId)
  )

pInstructionMethod :: CILParser Instruction
pInstructionMethod =
  (
    (Instruction_Call <$ pKeyword "call") <|>
    (Instruction_CallVirt <$ pKeyword "callvirt") <|>
    (Instruction_Jmp <$ pKeyword "jmp") <|>
    (Instruction_LdFtn <$ pKeyword "ldftn") <|>
    (Instruction_LdVirtFtn <$ pKeyword "ldvirtftn") <|>
    (Instruction_NewObj <$ pKeyword "newobj")
  )
  <*> pCallConv
  <*> pType
  <*> (pMb (pTypeSpec <* pPunc "::"))
  <*> pMethodName
  <*> pParamL

pInstructionField :: CILParser Instruction
pInstructionField =
  (
    (Instruction_LdFld <$ pKeyword "ldfld") <|>
    (Instruction_LdFldA <$ pKeyword "ldflda") <|>
    (Instruction_LdsFld <$ pKeyword "ldsfld") <|>
    (Instruction_LdsFldA <$ pKeyword "ldsflda") <|>
    (Instruction_StFld <$ pKeyword "stfld") <|>
    (Instruction_StsFld <$ pKeyword "stsfld")
  )
  <*> pType
  <*> pTypeSpec
  <* pPunc "::"
  <*> pId

pInstructionType :: CILParser Instruction
pInstructionType =
  (
    pAny (\(i,k) -> i <$ pKeyword k)
      [ (Instruction_Box, "box")
      , (Instruction_CastClass, "castclass")
      , (Instruction_CpObj, "cpobj")
      , (Instruction_InitObj, "initobj")
      , (Instruction_IsInst, "isinst")
      , (Instruction_LdElem, "ldelem")
      , (Instruction_LdElemA, "ldelema")
      , (Instruction_LdObj, "ldobj")
      , (Instruction_MkRefAny, "mkrefany")
      , (Instruction_NewArr, "newarr")
      , (Instruction_RefAnyVal, "refanyval")
      , (Instruction_SizeOf, "sizeof")
      , (Instruction_StElem, "stelem")
      , (Instruction_StObj, "stobj")
      , (Instruction_Unbox, "unbox")
      , (Instruction_UnboxAny, "unbox.any")
      ]
  ) <*> pTypeSpec

pInstructionString :: CILParser Instruction
pInstructionString =
  (Instruction_LdStr <$ pKeyword "ldstr") <*> pQSTRING

pInstruction :: CILParser Instruction
pInstruction =
  pInstructionNoOp <|>
  pInstructionParamLocal <|>
  pInstructionSingleInt <|>
  pInstructionSingleFloat <|>
  pInstructionBranch <|>
  pInstructionMethod <|>
  pInstructionField <|>
  pInstructionType <|>
  pInstructionString
%%]
