%%[0 hs
{-# LANGUAGE MagicHash, UnboxedTuples #-}
%%]
%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[1 module {%{EH}MSCIL.Parser} import(UU.Parsing, UHC.Util.ParseUtils, {%{EH}MSCIL}, Data.Char, {%{EH}MSCIL.Scanner}, Data.Bifunctor)
%%]

%%[(8 core) export(pType, pId, pDottedName, pParam, pParamL, pPrimitiveFFI, parseString)
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
  = (Type_GenMethodParam <$> (pPunc "!!" *> pInteger))
  <|> (Type_GenTypeParam <$> (pPunc "!" *> pInteger))
  <|> Type_Bool <$ pKeyword "bool"
  <|> Type_Char <$ pKeyword "char"
  <|> pKeyword "native" *>
    ( Type_NativeInt <$ pKeyword "int"
    <|> Type_NativeUInt <$ pKeyword "uint"
    )
  <|> Type_Object <$ pKeyword "object"
  <|> Type_String <$ pKeyword "string"
  <|> Type_String <$ pKeyword "void"

pType :: CILParser Type
pType = pSimpleType -- TODO: Expand

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

pPrimitiveFFI :: CILParser (Type, Id, ParamL)
pPrimitiveFFI = (\x y z -> (x,y,z)) <$> pType <*> pId <*> pParamL

pCallKind :: CILParser CallKind
pCallKind = CallKind_Default <$ pKeyword "default" -- TODO: Expand

pCallConv :: CILParser CallConv
pCallConv =
  CallConv_CallConv <$>
  (opt (True <$ pKeyword "instance") False) <*>
  (opt (True <$ pKeyword "explicit") False) <*>
  pCallKind

pInstructionNoOp :: CILParser Instruction
pInstructionNoOp =
  Instruction_Add <$ pKeyword "add" <|>
  Instruction_AddOvf <$ pKeyword "add.ovf" <|>
  Instruction_And <$ pKeyword "and" <|>
  Instruction_CEq <$ pKeyword "ceq" <|>
  Instruction_CGt <$ pKeyword "cgt" <|>
  Instruction_CGtUn <$ pKeyword "cgt.un" <|>
  Instruction_CLt <$ pKeyword "clt" <|>
  Instruction_CLtUn <$ pKeyword "clt.un" <|>
  Instruction_Div <$ pKeyword "div" <|>
  Instruction_DivUn <$ pKeyword "div.un" <|>
  Instruction_Dup <$ pKeyword "dup" <|>
  Instruction_LdNull <$ pKeyword "ldnull" <|>
  Instruction_Mul <$ pKeyword "mul" <|>
  Instruction_MulOvf <$ pKeyword "mul.ovf" <|>
  Instruction_MulOvfUn <$ pKeyword "mul.ovf.un" <|>
  Instruction_Neg <$ pKeyword "neg" <|>
  Instruction_Nop <$ pKeyword "nop" <|>
  Instruction_Not <$ pKeyword "not" <|>
  Instruction_Or <$ pKeyword "or" <|>
  Instruction_Pop <$ pKeyword "pop" <|>
  Instruction_Rem <$ pKeyword "rem" <|>
  Instruction_RemUn <$ pKeyword "rem.un" <|>
  Instruction_Ret <$ pKeyword "ret" <|>
  Instruction_Shl <$ pKeyword "shl" <|>
  Instruction_Shr <$ pKeyword "shr" <|>
  Instruction_ShrUn <$ pKeyword "shr.un" <|>
  Instruction_Sub <$ pKeyword "sub" <|>
  Instruction_SubOvf <$ pKeyword "sub.ovf" <|>
  Instruction_SubOvfUn <$ pKeyword "sub.ovf.un" <|>
  Instruction_Tail <$ pKeyword "tail." <|>
  Instruction_Xor <$ pKeyword "xor"

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
      , (Instruction_LdElemA, "ldelema")
      , (Instruction_LdObj, "ldobj")
      , (Instruction_MkRefAny, "mkrefany")
      , (Instruction_NewArr, "newarr")
      , (Instruction_RefAnyVal, "refanyval")
      , (Instruction_SizeOf, "sizeof")
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
  pInstructionField <|>
  pInstructionString

%%]
