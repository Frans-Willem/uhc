%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class interface around common functionality of Core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}AbstractCore.Utils} import ({%{EH}AbstractCore})
%%]

%%[(8 codegen) import({%{EH}Base.HsName.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}Ty})
%%]

%%[(8 codegen hmtyinfer) import({%{EH}Gam},{%{EH}Gam.ValGam},{%{EH}Gam.DataGam})
%%]

%%[(8 codegen) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(8 codegen) import(Data.List, qualified Data.Map as Map, qualified Data.Set as Set, Data.Maybe)
%%]

%%[(8 codegen) import(UHC.Util.Utils)
%%]

-- debug
%%[(8888 codegen) import({%{EH}Base.Debug},UHC.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstracted/shared types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(RCEEnv'(..),emptyRCEEnv)
-- | Env to support Reordering of Case Expression (RCE)
data RCEEnv' expr bind bindasp ty
  = RCEEnv
      { rceValGam           :: !ValGam                  -- type of value (amongst other)
      , rceTyVarMp          :: !VarMp                   -- tvar bindings for ValGam
      , rceDataGam          :: !DataGam                 -- data type + constructor info
      , rceCaseFailSubst    :: !(CSubst' expr bind bindasp ty)   -- fail continuation map
      , rceCaseIds          :: !UIDS                    -- fail ids
      , rceCaseCont         :: !expr                    -- continuation
      , rceEHCOpts          :: !EHCOpts                 -- options
      -- , rceIsStrict          :: !Bool                -- scrutinee must be evaluated
      }

emptyRCEEnv :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> RCEEnv' e b ba t
emptyRCEEnv opts = RCEEnv emptyGam emptyVarMp emptyGam Map.empty (Set.singleton uidStart) (acoreBuiltinUndefined opts) opts -- True
%%]

%%[(8 codegen) export(rceEnvDataAlts)
-- | All tags of the type of the constructor for a tag t
rceEnvDataAlts :: RCEEnv' e b ba t -> CTag -> Maybe [CTag]
rceEnvDataAlts env t
  = case t of
      CTag _ conNm _ _ _
         -> case valGamTyOfDataCon conNm (rceValGam env) of
              (_,ty,[])
                 -> dataGamTagsOfTy (rceTyVarMp env `varUpd` ty) (rceDataGam env)
              _  -> Nothing
                    -- panic ("rceEnvDataAlts: " ++ show conNm) -- Nothing
                    -- dataGamTagsOfTy (appCon conNm) (rceDataGam env)
      _  -> Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make pat from tag and arity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acorePatTagArityMbNms)
acorePatTagArityMbNms :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> CTag -> Int -> Maybe [HsName] -> p
acorePatTagArityMbNms opts ctag arity mbNmL
  = pat
  where pat = acorePatCon ctag (acorePatRestEmpty) (zipWith mkB nmL [0 .. arity - 1])
        mkB n o = acorePatFldTy (acoreTyErr "acorePatTagArityMbNms") (n,acoreInt opts o) n
        nmL = maybe (repeat hsnWild) id mbNmL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Saturate alt's of case w.r.t. all possible tags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

20100519 AD: to be sorted out further, especially the uncommented panic occurs because of lacking environmental info:

ehc: panic: acoreAltLSaturate.rceEnvDataAlts(1): CTag {ctagTyNm = UHC.Base.$Dict-Real, ctagNm = UHC.Base.$Dict-Real, ctagTag' = 0, ctagArity = 3, ctagMaxArity = 3}

Because these are single alternative records, it does not harm to assume that, but has to be sorted out.


%%[(8 codegen) export(acoreAltLSaturate)
acoreAltLSaturate :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> [a] -> [a]
acoreAltLSaturate env alts
  = case alts of
      (alt1:_) -> listSaturateWith 0 (length allAlts - 1) altIntTag allAlts alts
            where (allAlts,altIntTag)
                    = case acorePatMbCon pat of
                        -- if a con pat, use the tag to dispatch upon
                        Just (CTagRec,_,_)
                          -> ([(0,alt1)], const 0)
                        Just (tg,_,_)
                          -> case rceEnvDataAlts env tg of
                               Just ts    -> ([ (ctagTag t,mkA env t (ctagArity t)) | t <- ts ], ctagTag . panicJust "acoreAltLSaturate.rceEnvDataAlts(2)" . acoreAltMbTag)
                               _          -> -- tr "acoreAltLSaturate" (pp tg) $
                                             ([(0,alt1)], const 0)
                               -- _          -> panic ("acoreAltLSaturate.rceEnvDataAlts(1): " ++ show tg)
                        _ -> case acorePatMbInt pat of
                               -- if an int, use the int to dispatch upon; used internally only (by deriving Enum)
                               Just (_,i) -> ([ (fromInteger i, a) | a <- alts ], fromInteger . snd . panicJust "acoreAltLSaturate.acorePatMbInt(2)" . acorePatMbInt . fst . acoreUnAlt)
                               _          -> panic "acoreAltLSaturate.acorePatMbInt(1)"
                    where (pat,_) = acoreUnAlt alt1
                          mkA env ct a = acoreAlt (acorePatTagArityMbNms (rceEHCOpts env) ct a Nothing) (rceCaseCont env)
      _     -> []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extract offsets from pat bindings as separate binding to new/fresh names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
acorePatBindOffsetL :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> [pf] -> ([pf],[b])
acorePatBindOffsetL opts pbL
  =  let  (pbL',obL)
            =  unzip
               .  map
                    (\b -> let ((l,o),pbind) = acoreUnPatFld b
                               (n,_) = acoreUnBind pbind
                               offNm = hsnUniqify HsNameUniqifier_FieldOffset l
                           in  case acoreExprMbInt o of
                                 Just _ -> (b,[])
                                 _      -> (acorePatFldTy (acoreTyErr "acorePatBindOffsetL") (l,acoreVar offNm) n,[acoreBind1Ty offNm (acoreTyInt opts) o])
                    )
               $  pbL
     in   (pbL',concat obL)
%%]

%%[(8 codegen) export(acoreAltOffsetL)
acoreAltOffsetL :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> a -> (a,[b])
acoreAltOffsetL opts alt
  =  case acorePatMbCon p of
       Just (t,r,b)
         ->  (acoreAlt (acorePatCon t r b') e,offBL)
             where (b',offBL) = acorePatBindOffsetL opts b
       _ ->  (alt,[])
  where (p,e) = acoreUnAlt alt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construct case with: strict in expr, offsets strict
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(MbPatRest')
type MbPatRest' pr = Maybe (pr,Int) -- (pat rest, arity)
%%]

%%[(8 codegen) export(acoreStrictSatCaseTy)
-- | Make case expression from alternatives, saturating the alternatives w.r.t. all constructors
-- | Either:
-- |   - make a case expr from alternatives,
-- |     saturating the alternatives with defaults for missing alternatives.
-- |   - or, when only a single alternative binding a single field, bind it directly with a let
acoreStrictSatCaseTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe (HsName,t) -> e -> [a] -> e
acoreStrictSatCaseTy env mbNm e []
  = rceCaseCont env         -- TBD: should be error message "scrutinizing datatype without constructors"
acoreStrictSatCaseTy env mbNm e [alt]
  | isJust mbPatCon && length flds == 1 && not (ctagIsRec tg) && isJust mbDgi && dgiIsNewtype (fromJust mbDgi)
  = acoreLet cat
      ( [ acoreBind1CatTy cat pnm ty e ]
        ++ maybe [] (\(n,ty) -> [ acoreBind1CatTy cat n ty e ]) mbNm
      ) ae
  where mbDgi = dataGamLookup (ctagTyNm tg) (rceDataGam env)
        (pat,ae) = acoreUnAlt alt
        mbPatCon@(~(Just (tg,_,flds@(~([fld]))))) = acorePatMbCon pat
        (_,pbind) = acoreUnPatFld fld
        (pnm,_) = acoreUnBind pbind
        cat = acoreBindcategPlain
        ty = maybe (acoreTyErr "acoreStrictSatCaseTy.ty") snd mbNm
acoreStrictSatCaseTy env mbNm e alts
  = case mbNm of
      Just (n,ty)  -> acoreLet1StrictInTy n ty e $ mk alts
      Nothing -> mk alts e
  where mk (alt:alts) n
          = acoreLet (acoreBindcategStrict) altOffBL (acoreCaseDflt n (acoreAltLSaturate env (alt':alts)) (Just undef))
          where (alt',altOffBL) = acoreAltOffsetL (rceEHCOpts env) alt
        mk [] n
          = acoreCaseDflt n [] (Just undef) -- dummy case
        undef = acoreBuiltinUndefined (rceEHCOpts env)

acoreStrictSatCase :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe (HsName) -> e -> [a] -> e
acoreStrictSatCase env eNm e alts = acoreStrictSatCaseTy env (acoreTyErrLift "acoreStrictSatCase" eNm) e alts
{-# INLINE acoreStrictSatCase #-}
%%]

Export of the following group of defs can be removed after conversion of all utils to acore variants.

%%[(8 codegen)
-- | Make a case expr from non-saturated alternatives,
-- | alternatives are given by their tag + fields (name/offset) + rest (for extensible records) + alt expr
acoreSelsCasesTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe (HsName,t) -> e -> [(CTag,[(HsName,t,{-HsName,-}e)],MbPatRest' pr,e)] -> e
acoreSelsCasesTy env mbNm e tgSels
  = acoreStrictSatCaseTy env mbNm e alts
  where  alts = [ acoreAlt 
                    (acorePatCon ct
                       (mkRest mbRest ct)
                       [acorePatFldTy t (n,off) n | (n,t,{-lbl,-}off) <- nmLblOffL]
                    )
                    sel
                | (ct,nmLblOffL,mbRest,sel) <- tgSels
                ]
         mkRest mbr ct
           = case mbr of
               Just (r,_) -> r
               _          -> ctag (acorePatRestVar hsnWild) (\_ _ _ _ _ -> acorePatRestEmpty) ct

%%]

%%[(8 codegen) export(acoreSelsCaseTy)
-- | Make a case expr from a single alternative,
-- | the alternative given by their tag + fields (name/offset) + rest (for extensible records) + alt expr
acoreSelsCaseTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe (HsName,t) -> e -> CTag -> [(HsName,t,{-HsName,-}e)] -> MbPatRest' pr -> e -> e
acoreSelsCaseTy env ne e ct nmLblOffL mbRest sel = acoreSelsCasesTy env ne e [(ct,nmLblOffL,mbRest,sel)]
{-# INLINE acoreSelsCaseTy #-}
%%]

%%[(8 codegen) export(acoreSelCaseTy)
-- | Make a case expr from a single alternative with a single field,
-- | the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr
acoreSelCaseTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe (HsName,t) -> e -> CTag -> HsName -> e -> MbPatRest' pr -> e
acoreSelCaseTy env ne e ct n {-lbl-} off mbRest
  = acoreSelsCaseTy env ne e ct [(n,acoreTyErr $ "acoreSelCaseTy: " ++ show n,{-lbl,-}off)] mbRest (acoreVar n)

acoreSelCase :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe HsName -> e -> CTag -> HsName -> e -> MbPatRest' pr -> e
acoreSelCase env ne e ct n {-lbl-} off mbRest
  = acoreSelCaseTy env (acoreTyErrLift "acoreSelCase" ne) e ct n {-lbl-} off mbRest
{-# INLINE acoreSelCase #-}
%%]

%%[(8 codegen) export(acoreSatSelsCasesTy)
-- | Make a case expr from a single alternative with non-saturated fields,
-- | the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr,
-- | the fields (and alternatives) are saturated according to the tag + rest info
acoreSatSelsCasesTy
  :: forall e m b bound boundmeta bcat mbind t p pr pf a ba .
     (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe (HsName,t) -> e -> [(CTag,[(HsName,t,{-HsName,-}Int)],MbPatRest' pr,e)] -> e
acoreSatSelsCasesTy env ne e tgSels
  =  acoreSelsCasesTy env ne e alts
  where mkOffL ct mbr nol
          = case (ct,mbr) of
              (CTagRec       ,Nothing   ) -> map mklo nol
              (CTagRec       ,Just (_,a)) -> mkloL a
              (CTag _ _ _ a _,_         ) -> mkloL a
          where mklo :: (HsName,t,Int) -> (HsName,t,e)
                mklo (n,t,o) = (n,t,acoreInt opts o)
                mkloL :: Int -> [(HsName,t,e)]
                mkloL a = map mklo
                          $ listSaturateWith
                              0 (a-1)
                              (\(_,_,o) -> o)
                              [(o,(l,acoreTyErr $ "acoreSatSelsCasesTy.mkloL: " ++ show l,o)) | (o,l) <- zip [0..a-1] hsnLclSupply]
                              nol
        alts = [ (ct,mkOffL ct mbRest nmLblOffL,mbRest,sel) | (ct,nmLblOffL,mbRest,sel) <- tgSels ]
        opts = rceEHCOpts env
%%]

%%[(8 codegen) export(acoreSatSelsCaseTy)
-- | Make a case expr from a single alternative with non-saturated fields,
-- | the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr,
-- | the fields (and alternatives) are saturated according to the tag + rest info
acoreSatSelsCaseTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe (HsName,t) -> e -> CTag -> [(HsName,t,{-HsName,-}Int)] -> MbPatRest' pr -> e -> e
acoreSatSelsCaseTy env ne e ct nmLblOffL mbRest sel = acoreSatSelsCasesTy env ne e [(ct,nmLblOffL,mbRest,sel)]
{-# INLINE acoreSatSelsCaseTy #-}
%%]

%%[(8 codegen) hs export(acoreExprSatSelCaseTy)
-- | Make a case expr from a single alternative with a single field,
-- | the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr,
-- | the fields (and alternatives) are saturated according to the tag + rest info
acoreExprSatSelCaseTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe (HsName,t) -> e -> CTag -> HsName -> {- HsName -> -} Int -> MbPatRest' pr -> e
acoreExprSatSelCaseTy env ne e ct n {- lbl -} off mbRest = acoreSatSelsCaseTy env ne e ct [(n,acoreTyErr $ "acoreExprSatSelCaseTy: " ++ show n,{-lbl,-}off)] mbRest (acoreVar n)

acoreExprSatSelCase :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe (HsName) -> e -> CTag -> HsName -> {- HsName -> -} Int -> MbPatRest' pr -> e
acoreExprSatSelCase env ne e ct n {- lbl -} off mbRest = acoreExprSatSelCaseTy env (acoreTyErrLift "acoreExprSatSelCase" ne) e ct n {- lbl -} off mbRest
{-# INLINE acoreExprSatSelCase #-}
%%]

%%[(8888 codegen) export(acoreSatSelsCaseUpdTy)
-- | Make a case expr specifically for an update.
-- Not kept updated.
acoreSatSelsCaseUpdTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Maybe (HsName,t) -> e -> CTag -> Int -> [(Int,(e,m))] -> MbPatRest' pr -> e
acoreSatSelsCaseUpdTy env mbNm e ct arity offValL mbRest
  = acoreSatSelsCaseTy env mbNm e ct nmLblOffL mbRest sel
  where ns = take arity hsnLclSupply
        nmLblOffL = zip ns [0..] -- zip3 ns ns [0..]
        sel = acoreTagTupTy ct (acoreTyErr "AbstractCore.Utils.acoreSatSelsCaseUpdTy")
                $ map (fst.snd)
                $ listSaturateWith 0 (arity-1) fst [(o,(o,(acoreVar n,acoreMetavalDflt))) | (n,{-_,-}o) <- nmLblOffL] offValL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% List comprehension utilities for deriving, see also HS/ToEH
%%% These functions redo on the Core level the desugaring done in ToEH. Regretfully so ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 codegen) hs export(acoreMatchStringTy)
acoreMatchStringTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> String -> t -> e -> e -> e -> e
acoreMatchStringTy env str ty ok fail e
  = acoreLet1PlainTy x ty e
    $ foldr (\(c,ns@(_,xh,_)) ok
               -> matchCons ns
                  $ acoreMatchChar opts (Just $ hsnUniqifyEval xh)  c (acoreVar xh) ok fail
            )
            (matchNil xt ok)
    $ zip str nms
  where env' = env {rceCaseCont = fail}
        matchCons (x,xh,xt) e = acoreSatSelsCaseTy env' (Just (hsnUniqifyEval x,ty)) (acoreVar x) constag [(xh,acoreTyErr "acoreMatchStringTy.hd",0),(xt,acoreTyErr "acoreMatchStringTy.tl",1)] (Just (acorePatRestEmpty,2)) e
        matchNil   x        e = acoreSatSelsCaseTy env' (Just (hsnUniqifyEval x,ty)) (acoreVar x) niltag  []                                                                                    (Just (acorePatRestEmpty,0)) e
        constag = ctagCons opts
        niltag  = ctagNil  opts
        opts = rceEHCOpts env
        (nms@((x,_,_):_),(xt,_,_))
          = fromJust $ initlast $ snd
            $ foldr (\n (nt,l) -> (n,(n,hsnUniqifyStr HsNameUniqifier_Field "h" n,nt):l)) (hsnUnknown,[])
            $ take (length str + 1) $ hsnLclSupplyWith (mkHNmHidden "l")
%%]

%%[(99 codegen) hs export(acoreMatchTupleTy)
acoreMatchTupleTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> [HsName] -> t -> e -> e -> e
acoreMatchTupleTy env fldNmL ty ok e
  = acoreLet1PlainTy x ty e
    $ acoreSatSelsCaseTy env (Just (hsnUniqifyEval x,ty)) (acoreVar x) CTagRec (zipWith (\f o -> (f,acoreTyErr $ "acoreMatchTupleTy: " ++ show f,o)) fldNmL [0..]) (Just (acorePatRestEmpty,length fldNmL)) ok
  where x = mkHNmHidden "x"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs
data RCESplitCateg
  = RCESplitVar UIDS
  | RCESplitCon
  | RCESplitConMany
  | RCESplitConst
  | RCESplitIrrefutable
%%[[97
  | RCESplitBoolExpr
%%]]
  deriving Eq

rceSplitMustBeOnItsOwn :: RCESplitCateg -> Bool
rceSplitMustBeOnItsOwn RCESplitConMany     = True
rceSplitMustBeOnItsOwn RCESplitIrrefutable = True
rceSplitMustBeOnItsOwn _                   = False
%%]

%%[(8 codegen) hs
rceSplit :: (RAlt' e t b pr -> RCESplitCateg) -> RCEAltL' e t b pr -> [RCEAltL' e t b pr]
rceSplit f []   = []
rceSplit f [x]  = [[x]]
rceSplit f (x:xs@(x':_))
  | xcateg == f x'
    && not (rceSplitMustBeOnItsOwn xcateg)
      = let (z:zs) = rceSplit f xs
        in  (x:z) : zs
  | otherwise
      = [x] : rceSplit f xs
  where xcateg = f x
%%]

%%[(8 codegen) hs
-- | Add bindings from the name n of each alt to nm, to take care of different namings
rceRebinds :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => Bool -> (HsName,t) -> RCEAltL' e t b pr -> [b]
rceRebinds origOnly (nm,ty) alts
  = [ acoreBind1Ty n ty (acoreVar nm) | pn <- raltLPatNms alts, alsoUniq || rpatNmIsOrig pn, let n = rpatNmNm pn, n /= nm ]
  where alsoUniq = not origOnly
%%]

%%[(8 codegen) hs
rceMatchVar :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a, CSubstitutable e b ba t e) => RCEEnv' e b ba t ->  [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchVar env ((arg,ty):args') alts
  = remMatch
  where remMatch
          = rceMatchTy env args'
              [ RAlt_Alt remPats (mk $ acoreLet acoreBindcategPlain (rceRebinds True (arg',ty) [a]) e) f
              | a@(RAlt_Alt (RPat_Var _ _ mustEval : remPats) e f) <- alts
              , let (arg',mk) = if mustEval
                                then let argStrict = hsnUniqify HsNameUniqifier_Strict arg
                                     in  (argStrict,acoreLet1StrictTy argStrict ty (acoreVar arg))
                                else (arg,id)
              ]

rceMatchIrrefutable :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a, CSubstitutable e b ba t e) => RCEEnv' e b ba t ->  [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchIrrefutable env (argty@(arg,ty):args') alts@[RAlt_Alt (RPat_Irrefutable n _ b : remPats) e f]
  = acoreLet acoreBindcategPlain (rceRebinds False argty alts) $ acoreLet acoreBindcategPlain b remMatch
  where remMatch  = rceMatchTy env args' [RAlt_Alt remPats e f]

rceMkConAltAndSubAlts :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a, CSubstitutable e b ba t e) => RCEEnv' e b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> a
rceMkConAltAndSubAlts env ((arg,ty):args) alts@(alt:_)
  = acoreAlt altPat (acoreLet acoreBindcategPlain (rceRebinds True (arg,ty) alts) subMatch)
  where (subAlts,subAltSubs)
          =  unzip
               [ ( RAlt_Alt (pats ++ ps) e f
                 , map (\p -> let n = rpatNmNm (rcpPNm p) in (n,rcpTy p)) pats
                 )
               | (RAlt_Alt (RPat_Con _ _ _ (RPatConBind_One _ pbinds) : ps) e f) <- alts
               , let pats = [ p | (RPatFld_Fld _ _ _ p) <- pbinds ]
               ]
        subMatch
          =  rceMatchTy env (subAltSub ++ args) subAlts
          where subAltSub = zipWith (\(_,t) (n,ni) -> (ni,t)) (head subAltSubs) altNmIntroAssocL
        (altPat, altNmIntroAssocL)
          =  case alt of
               RAlt_Alt (RPat_Con n _ t (RPatConBind_One r pbL) : _) _ _
                 ->  (acorePatCon t r pbL', nmIntroAssocL)
                     where (pbL',nmIntroAssocL)
                               = unzip
                                   [ ( acorePatFldTy (rcpTy p) (l,o) introNm -- nm
                                     , (nm, introNm)
                                     )
                                   | (RPatFld_Fld l o n p, inx) <- zip pbL [(0 :: Int) ..]
                                   , let nm      = rpatNmNm $ rcpPNm p
                                   , let introNm = hsnUniqifyInt HsNameUniqifier_Field inx nm
                                   ]
        tyerr n = acoreTyErr ("rceMkConAltAndSubAlts: " ++ show n)

rceMatchCon :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a, CSubstitutable e b ba t e) => RCEEnv' e b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchCon env ((arg,ty):args) alts
  = acoreStrictSatCaseTy env (Just (arg',ty)) (acoreVar arg) alts'
  where arg'   =  hsnUniqifyEval arg
        alts'  =  map (rceMkConAltAndSubAlts env ((arg',ty):args))
                  $ groupSortOn (ctagTag . rcaTag)
                  $ filter (not . null . rcaPats)
                  $ alts

rceMatchConMany :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a, CSubstitutable e b ba t e) => RCEEnv' e b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchConMany env ((arg,ty):args) [RAlt_Alt (RPat_Con n _ t (RPatConBind_Many bs) : ps) e f]
  = acoreLet1StrictInTy arg' ty (acoreVar arg)
                        (\_ -> foldr (\mka e -> rceMatchTy env [(arg',ty)] (mka e)) (rceMatchTy env ((arg',ty):args) altslast) altsinit)
  where arg'     = hsnUniqifyEval arg
        altsinit = [ \e -> [RAlt_Alt (RPat_Con n ty t b     : []) e f] | b <- bsinit ]
        altslast =         [RAlt_Alt (RPat_Con n ty t blast : ps) e f]
        (bsinit,blast) = panicJust "rceMatchConMany" $ initlast bs

rceMatchConst :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a, CSubstitutable e b ba t e) => RCEEnv' e b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchConst env ((arg,ty):args) alts
  = acoreLet1StrictInTy arg' ty (acoreVar arg) (\n -> acoreLet cat (rceRebinds True (arg,ty) alts) (acoreCaseDflt n alts' (Just $ rceCaseCont env)))
  where arg' = hsnUniqifyEval arg
        alts' = [ acoreAlt (acoreRPat2Pat p) (cSubstApp (rceCaseFailSubst env) e {- tcSubstCaseAltFail (rceEHCOpts env) (rceCaseFailSubst env) e -}) | (RAlt_Alt (p:_) e _) <- alts ]
        cat = acoreBindcategPlain
%%]

%%[(97 codegen) hs
rceMatchBoolExpr :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a, CSubstitutable e b ba t e) => RCEEnv' e b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchBoolExpr env aargs@((arg,_):args) alts
  = foldr (\(n,c,t) f -> acoreIf (rceEHCOpts env) (Just n) c t f) (rceCaseCont env) alts'
  where alts'  =  map (\(u, alts@(RAlt_Alt (RPat_BoolExpr _ _ b _ : _) _ _ : _))
                         -> ( hsnUniqifyInt HsNameUniqifier_Evaluated u arg
                            , acoreApp b [acoreVar arg]
                            , rceMatchTy env args [ RAlt_Alt remPats e f | (RAlt_Alt (RPat_BoolExpr _ _ _ _ : remPats) e f) <- alts ]
                      )     )
                  $ zip [0..]
                  $ groupSortOn (rcpMbConst . head . rcaPats)
                  $ filter (not . null . rcaPats)
                  $ alts
%%]

%%[(8 codegen) hs
rceMatchSplits :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a, CSubstitutable e b ba t e) => RCEEnv' e b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchSplits env args alts@(alt:_)
  |  raltIsVar          alt  = rceMatchVar          env args alts
  |  raltIsConst        alt  = rceMatchConst        env args alts
  |  raltIsIrrefutable  alt  = rceMatchIrrefutable  env args alts
%%[[97
  |  raltIsBoolExpr     alt  = rceMatchBoolExpr     env args alts
%%]]
  |  raltIsConMany      alt  = rceMatchConMany      env args alts
  |  otherwise               = rceMatchCon          env args alts

%%]

%%[(8 codegen) hs export(rceMatchTy)
rceMatchTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a, CSubstitutable e b ba t e) => RCEEnv' e b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchTy env [] []    =  rceCaseCont env
rceMatchTy env [] alts  
  =  case [ e | (RAlt_Alt [] e _) <- alts ] of
       (e:_)  -> cSubstApp (rceCaseFailSubst env) e -- tcSubstCaseAltFail (rceEHCOpts env) (rceCaseFailSubst env) e
       _      -> rceCaseCont env
rceMatchTy env args alts
  =  foldr
        (\alts e
           ->  case acoreExprMbVar e of
                  Just _
                     ->  rceMatchSplits (rceUpdEnv e env) args alts
                  _  ->  acoreLet1PlainTy nc (rcpTy pc) e
                         $ rceMatchSplits (rceUpdEnv (acoreVar nc) env) args alts
                     where pc  = rcaPat $ head alts
                           nc  = hsnUniqify HsNameUniqifier_CaseContinuation (rpatNmNm $ rcpPNm pc)
        )
        (rceCaseCont env)
     $ (rceSplit (\a -> if      raltIsVar           a  then RCESplitVar (raaFailS a)
                        else if raltIsConst         a  then RCESplitConst
                        else if raltIsIrrefutable   a  then RCESplitIrrefutable
%%[[97
                        else if raltIsBoolExpr      a  then RCESplitBoolExpr
%%]]
                        else if raltIsConMany       a  then RCESplitConMany
                                                       else RCESplitCon
                 ) alts)
%%]

%%[(8888 codegen) hs export(rceMatch)
rceMatch :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a, CSubstitutable e b ba t e) => RCEEnv' e b ba t -> [(HsName)] -> RCEAltL' e t b pr -> e
rceMatch env args alts = rceMatchTy env (acoreTyErrLift "rceMatch" args) alts
%%]

%%[(8 codegen) hs export(rceUpdEnv)
rceUpdEnv :: e -> RCEEnv' e b ba t -> RCEEnv' e b ba t
rceUpdEnv e env
  = env { rceCaseFailSubst = Map.union (acoreCSubstFromUidExprL [ (i,e) | i <- Set.toList (rceCaseIds env) ])
                             $ rceCaseFailSubst env
        , rceCaseCont      = e
        }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CSubst construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
%%[(9 codegen) hs export(acoreCSubstFromVarMpImpls)
acoreCSubstFromVarMpImpls :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => VarMp -> CSubst' e b ba t
acoreCSubstFromVarMpImpls c
  =  acoreCSubstFromUidImplsL
        [ (iv,(acoreCoeImplsApp i,acoreCoeImplsLam acoreCoeId i))
        | (iv,VMIImpls i) <- varmpToAssocL c, let (_,mbTl) = implsPredsMbTail i, isNothing mbTl
        ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RPatFld -> binds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreRPatBindL2BindL)
acoreRPatBindL2BindL :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => RCEEnv' e b ba t -> Bool -> HsName -> CTag -> MbPatRest' pr -> AssocL (RPatFld' e t b pr) (Maybe Int) -> [b]
acoreRPatBindL2BindL env hasSub parNm ct rest pbL 
  = concat
    $  map  (\(RPatFld_Fld l o _ p,mbOff)
                -> let  b n = [acoreBind1CatTy acoreBindcategPlain n (rcpTy p) (mkc n mbOff)]
                        pn  = parNm
                        pn' = hsnUniqifyEval pn
                        mkc n (Just o) = acoreExprSatSelCaseTy env (Just (pn',ty pn')) (acoreVar pn) ct n {- l -} o rest
                        mkc n Nothing  = acoreSelCaseTy        env (Just (pn',ty pn')) (acoreVar pn) ct n {- l -} o rest
                        ty n = acoreTyErr ("acoreRPatBindL2BindL: " ++ show n)
                   in   case rcpPNm p of
                            RPatNmOrig n           -> b n
                            RPatNmUniq n | hasSub  -> b n
                            _                      -> []
            )
    $  pbL
%%]




