%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[1 module {%{EH}MSCIL.FFI} import({%{EH}MSCIL}, {%{EH}MSCIL.Parser}, UU.Parsing, Data.Char, UHC.Util.ParseUtils, Data.Maybe)
%%]

%%[(8 core)
{- Parsing -}
type FFIType = ([(TypeSpec, Maybe Int)], [TypeSpec])
pFFI :: CILParser [(Either Instruction String, Maybe FFIType)]
pFFI = pListSep (pPunc ";") pSingleFFI

pSingleFFI :: CILParser (Either Instruction String, Maybe FFIType)
pSingleFFI = (\x y -> (x,y)) <$> (Left <$> pInstruction <|> Right <$> pSQSTRING) <*> (pMb (pKeyword ".override" *> pFFIType))

pFFIType :: CILParser FFIType
pFFIType =
  (\x y -> (y,x))
     <$> (
      (pPacked (pPunc "(") (pPunc ")") $ pListSep (pPunc ",") (TypeSpec_Type <$> pType)) <|>
      ((:[]) <$> pTypeSpec)
    )
    <*> ((map paramToFFIParam) <$> pParamL)

splitFFIParam :: Param -> (Param, (TypeSpec, Maybe Int))
splitFFIParam param@(Param_Param attrs ty Nothing) = (param, (TypeSpec_Type ty, Nothing))
splitFFIParam param@(Param_Param attrs ty (Just (Id_Id paramid)))
  = (Param_Param attrs ty (fmap Id_Id newid), (TypeSpec_Type ty, mbIndex))
  where (newid, mbIndex) = splitFFIParamName paramid

splitFFIParamName :: String -> (Maybe String, Maybe Int)
splitFFIParamName complete@('%':rest)
  = if ((all isDigit precolon) && (not (null precolon)))
      then
        if (length colonrest <= 1)
          then (Nothing, Just (read precolon))
          else (Just (tail colonrest), Just (read precolon))
      else (Just complete, Nothing)
  where (precolon, colonrest) = span (/=':') rest
splitFFIParamName s = (Just s, Nothing)

paramToFFIParam :: Param -> (TypeSpec, Maybe Int)
paramToFFIParam = (\(x,y) -> y) . splitFFIParam

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (x,y) = (x, f y)

parseFFI :: String -> [(MethodBodyItemL, Int)] -> (MethodBodyItemL, Int)
parseFFI str args =
  case (parseString pFFI str) of
    Right ffi ->
      ( (concat paraminstrs)
        ++ (concat (map (ffiToBodyItem . fst) finalffi))
        ++ boxinstrs
      , parammaxstack `max` ffimaxstack `max` boxmaxstack
      )
      where
        finalffi = map ((mapSnd ffiCleanParamInfo) . ffiForceParamInfo . ffiExtractParamInfo) ffi
        (params, rets) = foldr combineFFIType ([],[]) (map snd finalffi)
        indexedParams = zipWith (\(ty,i) di -> (ty, fromMaybe di i)) params [0..]
        (paraminstrs, parammaxstacks) = unzip $ map (\(ty,i) -> readParam args i ty) indexedParams
        parammaxstack = foldr max (length parammaxstacks) (zipWith (+) parammaxstacks [0..])
        (_,ffimaxstack) = foldl ffiTypeMaxStack (length params, length params) (map snd finalffi)
        (boxinstrs, boxmaxstack) = boxInstructions rets
    Left msg -> error msg

readParam :: [(MethodBodyItemL, Int)] -> Int -> TypeSpec -> (MethodBodyItemL, Int)
readParam args index ty | index >= length args = error $ "FFI Argument " ++ (show index) ++ " out of range"
readParam args index ty | otherwise
  = (arginstrs ++ unboxinstrs, max argmaxstack (unboxmaxstack + 1))
  where
    (arginstrs, argmaxstack) = args !! index
    (unboxinstrs, unboxmaxstack) = unboxTypeSpec ty

boxInstructions :: [TypeSpec] -> (MethodBodyItemL, Int)
boxInstructions [] = ([MethodBodyItem_Instr Instruction_LdNull], 1)
boxInstructions (x:xs)
  = (pops ++ boxinstr, 1 + boxmaxstack)
  where
    numpops = length xs
    pops = take numpops (repeat (MethodBodyItem_Instr Instruction_Pop))
    (boxinstr, boxmaxstack) = boxTypeSpec x

boxTypeSpec :: TypeSpec -> (MethodBodyItemL, Int)
boxTypeSpec (TypeSpec_External _) = ([], 0)
boxTypeSpec (TypeSpec_Ref ref) = ([], 0)
boxTypeSpec (TypeSpec_Type ty) = boxType ty

boxType :: Type -> (MethodBodyItemL, Int)
boxType (Type_GenTypeParam _) = ([], 0)
boxType (Type_GenMethodParam _) = ([], 0)
boxType (Type_Class _) = ([], 0)
boxType (Type_Method _ _ _) = ([], 0)
boxType (Type_Object) = ([], 0)
boxType (Type_String) = ([], 0)
boxType (Type_ManagedPtr _) = ([], 0)
boxType (Type_UnmanagedPtr _) = ([], 0)
boxType (Type_GenType _ _) = ([], 0)
boxType (Type_Array _ _) = ([] ,0)
boxType (Type_ModOpt ty _) = boxType ty
boxType (Type_ModReq ty _) = boxType ty
boxType (Type_Pinned ty) = boxType ty
boxType (Type_TypedRef) = ([], 0)
boxType (Type_Valuetype _) = ([] ,0)
boxType (Type_Void) = ([], 0)
boxType other = ([MethodBodyItem_Instr (Instruction_Box (TypeSpec_Type other))], 0)

unboxTypeSpec :: TypeSpec -> (MethodBodyItemL, Int)
unboxTypeSpec (TypeSpec_External _) = ([], 0)
unboxTypeSpec (TypeSpec_Ref ref) = ([], 0)
unboxTypeSpec (TypeSpec_Type ty) = unboxType ty

unboxType :: Type -> (MethodBodyItemL, Int)
unboxType (Type_GenTypeParam _) = ([], 0)
unboxType (Type_GenMethodParam _) = ([], 0)
unboxType (Type_Class _) = ([], 0)
unboxType (Type_Method _ _ _) = ([], 0)
unboxType (Type_Object) = ([], 0)
unboxType (Type_String) = ([], 0)
unboxType (Type_ManagedPtr _) = ([], 0)
unboxType (Type_UnmanagedPtr _) = ([], 0)
unboxType (Type_GenType _ _) = ([], 0)
unboxType (Type_Array _ _) = ([] ,0)
unboxType (Type_ModOpt ty _) = unboxType ty
unboxType (Type_ModReq ty _) = unboxType ty
unboxType (Type_Pinned ty) = unboxType ty
unboxType (Type_TypedRef) = ([], 0)
unboxType (Type_Valuetype _) = ([] ,0)
unboxType (Type_Void) = ([], 0)
unboxType other = ([MethodBodyItem_Instr (Instruction_UnboxAny (TypeSpec_Type other))], 0)

ffiCleanParamInfo :: FFIType -> FFIType
ffiCleanParamInfo (params, rets) = (filter (\(x,y) -> x /= TypeSpec_Type Type_Void) params, filter (/= TypeSpec_Type Type_Void) rets)

ffiForceParamInfo :: (Either Instruction String, Maybe FFIType) -> (Either Instruction String, FFIType)
ffiForceParamInfo (x, Nothing) = error "Not all FFI type information present"
ffiForceParamInfo (x, Just y) = (x,y)

ffiExtractParamInfo :: (Either Instruction String, Maybe FFIType) -> (Either Instruction String, Maybe FFIType)
-- If type info is already present, don't edit
ffiExtractParamInfo current@(_, Just _) = current
ffiExtractParamInfo current@(Right _, _) = current
ffiExtractParamInfo current@(Left i, _) = case (instrExtractParamInfo i) of (x, y) -> (Left x, y)

simpleFFIType :: Instruction -> [Type] -> [Type] -> (Instruction, Maybe FFIType)
simpleFFIType instr args ret = (instr, Just (map (\x -> (TypeSpec_Type x, Nothing)) args, map TypeSpec_Type ret))

instrExtractParamInfo :: Instruction -> (Instruction,Maybe FFIType)
instrExtractParamInfo cur@Instruction_Add = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_AddOvf = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_And = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_CEq = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_Int32]
instrExtractParamInfo cur@Instruction_CGt = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_Int32]
instrExtractParamInfo cur@Instruction_CGtUn = simpleFFIType cur [Type_NativeUInt, Type_NativeUInt] [Type_Int32]
instrExtractParamInfo cur@Instruction_CLt = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_Int32]
instrExtractParamInfo cur@Instruction_CLtUn = simpleFFIType cur [Type_NativeUInt, Type_NativeUInt] [Type_Int32]
instrExtractParamInfo cur@Instruction_Div = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_DivUn = simpleFFIType cur [Type_NativeUInt, Type_NativeUInt] [Type_NativeUInt]
instrExtractParamInfo cur@Instruction_Dup = simpleFFIType cur [Type_Object] [Type_Object, Type_Object]
instrExtractParamInfo cur@Instruction_LdNull = simpleFFIType cur [] [Type_Object]
instrExtractParamInfo cur@Instruction_Mul = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_MulOvf = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_MulOvfUn = simpleFFIType cur [Type_NativeUInt, Type_NativeUInt] [Type_NativeUInt]
instrExtractParamInfo cur@Instruction_Neg = simpleFFIType cur [Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_Nop = simpleFFIType cur [] []
instrExtractParamInfo cur@Instruction_Not = simpleFFIType cur [Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_Or = simpleFFIType cur [Type_Bool, Type_Bool] [Type_Bool]
instrExtractParamInfo cur@Instruction_Pop = simpleFFIType cur [Type_Object] []
instrExtractParamInfo cur@Instruction_Rem = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_RemUn = simpleFFIType cur [Type_NativeUInt, Type_NativeUInt] [Type_NativeUInt]
instrExtractParamInfo cur@Instruction_Ret = simpleFFIType cur [] []
instrExtractParamInfo cur@Instruction_Shl = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_Shr = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_ShrUn = simpleFFIType cur [Type_NativeUInt, Type_NativeUInt] [Type_NativeUInt]
instrExtractParamInfo cur@Instruction_Sub = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_SubOvf = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@Instruction_SubOvfUn = simpleFFIType cur [Type_NativeUInt, Type_NativeUInt] [Type_NativeUInt]
instrExtractParamInfo cur@Instruction_Tail = simpleFFIType cur [] []
instrExtractParamInfo cur@Instruction_Xor = simpleFFIType cur [Type_NativeInt, Type_NativeInt] [Type_NativeInt]
instrExtractParamInfo cur@(Instruction_LdArg _) = simpleFFIType cur [] [Type_Object]
instrExtractParamInfo cur@(Instruction_LdArgA _) = simpleFFIType cur [] [Type_ManagedPtr Type_Object]
instrExtractParamInfo cur@(Instruction_LdLoc _) = simpleFFIType cur [] [Type_Object]
instrExtractParamInfo cur@(Instruction_LdLocA _) = simpleFFIType cur [] [Type_ManagedPtr Type_Object]
instrExtractParamInfo cur@(Instruction_StArg _) = simpleFFIType cur [Type_Object] []
instrExtractParamInfo cur@(Instruction_StLoc _) = simpleFFIType cur [Type_Object] []
instrExtractParamInfo cur@(Instruction_LdcI4 _) = simpleFFIType cur [] [Type_Int32]
instrExtractParamInfo cur@(Instruction_Unaligned _) = simpleFFIType cur [] []
instrExtractParamInfo cur@(Instruction_LdcI8 _) = simpleFFIType cur [] [Type_Int64]
instrExtractParamInfo cur@(Instruction_LdcR4 _) = simpleFFIType cur [] [Type_Float32]
instrExtractParamInfo cur@(Instruction_LdcR8 _) = simpleFFIType cur [] [Type_Float64]
instrExtractParamInfo cur@(Instruction_BrFalse _) = simpleFFIType cur [Type_Bool] []
instrExtractParamInfo cur@(Instruction_BrTrue _) = simpleFFIType cur [Type_Bool] []
instrExtractParamInfo cur@(Instruction_Br _) = simpleFFIType cur [] []
-- TODO: Some instructions
instrExtractParamInfo cur@(Instruction_LdStr _) = simpleFFIType cur [] [Type_String]

instrExtractParamInfo cur@(Instruction_Call conv ret cls name params)
  = (Instruction_Call conv ret cls name newparams, Just (ffiparams, [TypeSpec_Type ret]))
  where
    (_, ffiparams) = unzip (map splitFFIParam (maybeAddInstanceArg conv cls params))
    (newparams, _) = unzip (map splitFFIParam params)

instrExtractParamInfo cur@(Instruction_CallVirt conv ret cls name params)
  = (Instruction_CallVirt conv ret cls name newparams, Just (ffiparams, [TypeSpec_Type ret]))
  where
    (_, ffiparams) = unzip (map splitFFIParam (maybeAddInstanceArg conv cls params))
    (newparams, _) = unzip (map splitFFIParam params)

instrExtractParamInfo cur@(Instruction_Jmp conv ret cls name params)
  = (Instruction_Jmp conv ret cls name newparams, Just (ffiparams, []))
  where
    (_, ffiparams) = unzip (map splitFFIParam (maybeAddInstanceArg conv cls params))
    (newparams, _) = unzip (map splitFFIParam params)

instrExtractParamInfo cur@(Instruction_LdFtn conv ret cls name params)
  = (Instruction_LdFtn conv ret cls name params, Just ([], [TypeSpec_Type (Type_Method conv ret (maybeAddInstanceArg conv cls params))]))

instrExtractParamInfo cur@(Instruction_LdVirtFtn conv ret cls name params)
  = (Instruction_LdVirtFtn conv ret cls name params, Just (ffiparams, [TypeSpec_Type (Type_Method conv ret (maybeAddInstanceArg conv cls params))]))
  where
    (_, ffiparams) = unzip (map splitFFIParam (maybeAddInstanceArg conv cls []))

instrExtractParamInfo cur@(Instruction_NewObj conv ret cls name params)
  = (Instruction_NewObj conv ret cls name newparams, Just (ffiparams, mbToList cls))
  where
    (newparams, ffiparams) = unzip (map splitFFIParam params)
    mbToList Nothing = []
    mbToList (Just x) = [x]

instrExtractParamInfo cur@(Instruction_Box cls) = (cur, Just ([(cls,Nothing)],[TypeSpec_Type Type_Object]))
instrExtractParamInfo cur@(Instruction_LdElem cls)
  = (cur, fmap (\ty -> (map (\t -> (t, Nothing)) [TypeSpec_Type (Type_Array ty []),TypeSpec_Type Type_NativeInt],[cls])) (typeSpec2MbType cls))
instrExtractParamInfo cur@(Instruction_NewArr cls)
  = (cur, fmap (\ty -> ([(TypeSpec_Type Type_NativeInt, Nothing)],[TypeSpec_Type (Type_Array ty [])])) (typeSpec2MbType cls))
instrExtractParamInfo cur@(Instruction_StElem cls)
  = (cur, fmap (\ty -> (map (\t -> (t, Nothing)) [TypeSpec_Type (Type_Array ty []),TypeSpec_Type Type_NativeInt, cls],[])) (typeSpec2MbType cls))
instrExtractParamInfo cur@(Instruction_Unbox cls) = (cur, Just ([(TypeSpec_Type Type_Object,Nothing)],[cls]))
instrExtractParamInfo cur@(Instruction_UnboxAny cls)
  = (cur, fmap (\ty -> ([(TypeSpec_Type Type_Object,Nothing)],[TypeSpec_Type (Type_ManagedPtr ty)])) (typeSpec2MbType cls))

instrExtractParamInfo cur = error ("No FFI parameter info known for instruction " ++ (show cur))

typeSpec2MbType :: TypeSpec -> Maybe Type
typeSpec2MbType (TypeSpec_Type ty) = Just ty
typeSpec2MbType (TypeSpec_Ref ref) = Just (Type_Class ref)
typeSpec2MbType (TypeSpec_External _) = Nothing
    

maybeAddInstanceArg :: CallConv -> MbTypeSpec -> ParamL -> ParamL
maybeAddInstanceArg (CallConv_CallConv True _ _) (Just (TypeSpec_Type ty)) = ((Param_Param [] ty Nothing):)
maybeAddInstanceArg (CallConv_CallConv True _ _) (Just (TypeSpec_Ref ref)) = ((Param_Param [] (Type_Class ref) Nothing):)
maybeAddInstanceArg _ _ = id


ffiToBodyItem :: Either Instruction String -> MethodBodyItemL
ffiToBodyItem (Left i) = [MethodBodyItem_Instr i]
ffiToBodyItem (Right s) = [MethodBodyItem_Raw s]

combineFFIType :: FFIType -> FFIType -> FFIType
combineFFIType (popa, pusha) (popb, pushb)
  = (needb ++ popa, lefta ++ pushb)
  where
    lefta = reverse (drop (length popb) (reverse pusha))
    needb = reverse (drop (length pusha) (reverse popb))

ffiTypeMaxStack :: (Int, Int) -> FFIType -> (Int, Int)
ffiTypeMaxStack (curstack, maxstack) (pop, push) 
  = (curstack - popl + pushl, maxstack `max` (curstack - popl) `max` (curstack - popl + pushl))
  where
    popl = length pop
    pushl = length push

%%]
