%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HI parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 module {%{EH}HI.Parser} import(UU.Parsing, EH.Util.ParseUtils(PlainParser), EH.Util.ScanUtils, {%{EH}Base.Common})
%%]

%%[20 import( {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.Parser}, {%{EH}HI})
%%]

%%[(20 hmtyinfer || hmtyast) import( {%{EH}Ty})
%%]

%%[20 import({%{EH}HS.Parser}(pFixity))
%%]

%%[(20 hmtyinfer || hmtyast) import({%{EH}Ty.Parser})
%%]

%%[(20 codegen) import({%{EH}Core.Parser}(pCExpr))
%%]

%%[(20 codegen grin) import({%{EH}GrinCode.Parser}(pExprSeq))
%%]

%%[(20 hmtyinfer) import(qualified {%{EH}Pred} as Pr,{%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}Pred.CHR},{%{EH}Pred.CommonCHR})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
type HIParser       hp     =    PlainParser Token hp

%%]

%%[(20 hmtyinfer)
pLabelOffset :: HIParser LabelOffset
pLabelOffset = pKeyTk "offset" *> (LabelOffset_Off <$> pInt <|> LabelOffset_Var <$> pUIDHI)

pTyKiKey :: HIParser TyKiKey
pTyKiKey = TyKiKey_Name <$> pDollNm <|> TyKiKey_TyVar <$> pUIDHI

pLabel :: HIParser Label
pLabel = pKeyTk "label" *> (Label_Lab <$> pDollNm <|> Label_Var <$> pUIDHI)

pPredScope :: HIParser PredScope
pPredScope = pKeyTk "scope" *> ((PredScope_Lev . rllFromList) <$> pBracks_pCommas pInt <|> PredScope_Var <$> pUIDHI)

pCHRPredOcc :: HIParser CHRPredOcc
pCHRPredOcc = mkCHRPredOcc <$ pOCURLY <*> pPred <* pCOMMA <*> pPredScope <* pCCURLY

%%]

%%[20 export(pAGItf)
pNmIs :: String -> HIParser HsName
pNmIs k = pKeyTk k *> pDollNm <* pEQUAL

pAGItf :: HIParser AGItf
pAGItf
  = AGItf_AGItf <$> pModule

pModule :: HIParser Module
pModule
  = Module_Module <$ pMODULE <*> pDollNm <* pEQUAL <* pOCURLY <*> pListSep pSEMI pBinding <* pCCURLY

pVarUIDHsName :: HIParser VarUIDHsName
pVarUIDHsName
  =   VarUIDHs_Name <$ pKeyTk "varuidnmname" <*  pOCURLY <*> pUIDHI <* pCOMMA <*> pDollNm <* pCCURLY
  <|> VarUIDHs_UID  <$ pKeyTk "varuidnmuid"  <*> pUIDHI
  <|> VarUIDHs_Var  <$ pKeyTk "varuidnmvar"  <*> pUIDHI
%%]

%%[(20 hmtyinfer)
pConstraint :: HIParser (Constraint CHRPredOcc RedHowAnnotation)
pConstraint
  =   Prove     <$ pKeyTk "Prove"     <* pOCURLY <*> pCHRPredOcc <* pCCURLY
  <|> Assume    <$ pKeyTk "Assume"    <* pOCURLY <*> pCHRPredOcc <* pCCURLY
  <|> Reduction <$ pKeyTk "Reduction" <* pOCURLY <*> pCHRPredOcc
                                      <* pCOMMA  <*> pRedHowAnnotation
                                      <* pCOMMA  <*> pCurlyCommaBlock pCHRPredOcc
                                      <* pCCURLY

pGuard :: HIParser Guard
pGuard
  =   (\[sc1,sc2,sc3] -> HasStrictCommonScope   sc1 sc2 sc3) <$ pKeyTk "HasStrictCommonScope"  <*> pCurlyCommaBlock pPredScope
  <|> (\[sc1,sc2,sc3] -> IsStrictParentScope    sc1 sc2 sc3) <$ pKeyTk "IsStrictParentScope"   <*> pCurlyCommaBlock pPredScope
  <|> (\[sc1,sc2]     -> IsVisibleInScope       sc1 sc2    ) <$ pKeyTk "IsVisibleInScope"      <*> pCurlyCommaBlock pPredScope
  <|> (\[sc1,sc2]     -> NotEqualScope          sc1 sc2    ) <$ pKeyTk "NotEqualScope"         <*> pCurlyCommaBlock pPredScope
  <|> (\[sc1,sc2]     -> EqualScope             sc1 sc2    ) <$ pKeyTk "EqualScope"            <*> pCurlyCommaBlock pPredScope
  <|>                    NonEmptyRowLacksLabel               <$ pKeyTk "NonEmptyRowLacksLabel" <* pOCURLY <*> pTy
                                                                                               <* pCOMMA  <*> pLabelOffset
                                                                                               <* pCOMMA  <*> pTy
                                                                                               <* pCOMMA  <*> pLabel
                                                                                               <* pCCURLY

pRedHowAnnotation :: HIParser RedHowAnnotation
pRedHowAnnotation
  =   RedHow_ByInstance   <$ pKeyTk "redhowinst"   <* pOCURLY <*> pDollNm
                                                   <* pCOMMA  <*> pPred
                                                   <* pCOMMA  <*> pPredScope
                                                   <* pCCURLY
  <|> RedHow_BySuperClass <$ pKeyTk "redhowsuper"  <* pOCURLY <*> pDollNm
                                                   <* pCOMMA  <*> pInt
                                                   <* pCOMMA  <*> pCTag
                                                   <* pCCURLY
  <|> RedHow_ProveObl     <$ pKeyTk "redhowprove"  <* pOCURLY <*> pUIDHI
                                                   <* pCOMMA  <*> pPredScope
                                                   <* pCCURLY
  <|> RedHow_Assumption   <$ pKeyTk "redhowassume" <* pOCURLY <*> pVarUIDHsName
                                                   <* pCOMMA  <*> pPredScope
                                                   <* pCCURLY
  <|> RedHow_ByScope      <$ pKeyTk "redhowscope"
  <|> RedHow_ByLabel      <$ pKeyTk "redhowlabel"  <* pOCURLY <*> pLabel
                                                   <* pCOMMA  <*> pLabelOffset
                                                   <* pCOMMA  <*> pPredScope
                                                   <* pCCURLY

  <|> RedHow_Lambda       <$ pKeyTk "redhowlambda" <* pOCURLY <*> pUIDHI
                                                   <* pCOMMA  <*> pPredScope
                                                   <* pCCURLY
%%]

%%[20
pBinding :: HIParser Binding
pBinding
  =   Binding_Fixity    <$> pNmIs "fixity"   <* pOCURLY <*> pInt    <* pSEMI <*> pFixity  <* pCCURLY
  <|> Binding_Stamp     <$  pNmIs "stamp"    <* pOCURLY <*> pString <* pSEMI <*> pString
                                             <* pSEMI   <*> pString <* pSEMI <*> pString
                                             <* pSEMI   <*> pString <* pSEMI <*> pString
                                             <* pSEMI   <*> pString
                                             <* pSEMI   <*> (read <$> pInteger)
                                             <* pCCURLY
  <|> Binding_ImportModules
                        <$  pNmIs "importmodules"
                                             <* pOCURLY <*> pCurlySemiBlock pDollNm
                                             <* pSEMI   <*> pCurlySemiBlock pDollNm
                                             <* pCCURLY
  <|> Binding_Export    <$  pNmIs "export"   <* pOCURLY <*> (VisibleNo <$ pKeyTk "visibleno" <|> VisibleYes <$ pKeyTk "visibleyes") <* pSEMI <*> pModEntRel  <* pCCURLY
  <|> Binding_Ids       <$  pNmIs "iddef"    <* pOCURLY
                                             <*> pListSep pSEMI ((,) <$ pOCURLY <*> pIdOcc <* pSEMI <*> pIdOcc <* pCCURLY)
                                             <* pCCURLY
%%[[(20 hmtyinfer)
  <|> Binding_TyKinds   <$  pNmIs "tykind"   <* pOCURLY
                                             <*> pListSep pSEMI ((,) <$ pOCURLY <*> pTyKiKey <* pSEMI <*> pTy <* pCCURLY)
                                             <* pCCURLY
  <|> Binding_TyPolarities
                        <$  pNmIs "typolarity"
                                             <* pOCURLY
                                             <*> pListSep pSEMI ((,) <$ pOCURLY <*> pDollNm <* pSEMI <*> pTy <* pCCURLY)
                                             <* pCCURLY
  <|> Binding_Val       <$> pNmIs "value"    <* pOCURLY <*> pTy <* pCCURLY
  <|> Binding_Ty        <$> pNmIs "type"     <* pOCURLY <*> pTy <* pCCURLY
  <|> (\tn ty cs -> Binding_DataCon tn ty (map (\f -> f tn) cs))
      <$> pNmIs "data" <*  pOCURLY
                       <*> pTy <* pSEMI
                       <*> pCurlySemiBlock
                             ((\n t a ma fm tn -> (n,(CTag tn n t a ma,fm)))
                              <$> pDollNm   <*  pEQUAL
                              <*  pOCURLY   <*> pInt <* pCOMMA <*> pInt <* pCOMMA <*> pInt
                                            <*> pList ((,) <$ pSEMI <*> pDollNm <* pEQUAL <*> pInt)
                              <*  pCCURLY
                             ) <* pSEMI
                       <*> pBool
                       <*  pCCURLY
  <|> Binding_Class     <$> pNmIs "class"       <* pOCURLY <*> pTy <* pSEMI <*> pTy <* pSEMI <*> pDollNm <* pCCURLY
  <|> Binding_CHRStore  <$  pNmIs "chrstore"    <*> pCurlySemiBlock
                                                      (CHR <$ pOCURLY <*> pCurlySemiBlock pConstraint
                                                           <* pSEMI   <*> pInt
                                                           <* pSEMI   <*> pCurlySemiBlock pGuard
                                                           <* pSEMI   <*> pCurlySemiBlock pConstraint
                                                           <* pCCURLY
                                                      )
%%]]
%%[[(20 codegen)
  <|> Binding_Arities   <$  pNmIs "arity"    <*> pCurlySemiBlock ((,) <$ pOCURLY <*> pDollNm <* pSEMI <*> pInt <* pCCURLY)
%%]]
%%[[(20 codegen grin)
  <|> Binding_GrInlines <$  pNmIs "grInline" <*> pCurlySemiBlock ((\n a g -> (n,(a,g))) <$ pOCURLY <*> pDollNm <* pSEMI <*> pCurlySemiBlock pDollNm <* pSEMI <*> pCurlys pExprSeq <* pCCURLY)
%%]]
%%]
