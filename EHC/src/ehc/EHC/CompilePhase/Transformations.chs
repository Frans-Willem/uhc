%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Interface/wrapper to various transformations for Core, TyCore, etc.
%%]

%%[8 module {%{EH}EHC.CompilePhase.Transformations}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]

%%[8 import(Control.Monad.State)
%%]

%%[8 import(UHC.Util.Lens)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[(8 codegen) import({%{EH}Base.Optimize})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]
%%[99 import({%{EH}EHC.CompilePhase.Module(cpUpdHiddenExports)})
%%]
%%[8 import(qualified {%{EH}Config} as Cfg)
%%]

-- AST handling
%%[8 import({%{EH}EHC.ASTHandler.Instances})
%%]

-- Transformation utils
%%[(8 codegen) import({%{EH}CodeGen.TrfUtils})
%%]

-- Core transformations
%%[(8 codegen) import({%{EH}Core.Trf})
%%]

-- JavaScript transformations
%%[(8 javascript) import({%{EH}JavaScript.Trf})
%%]

-- Cmm transformations
%%[(8 codegen cmm) import({%{EH}Cmm.Trf})
%%]

-- CoreCPS transformations
%%[(8 core) import({%{EH}CoreCPS.Trf})
%%]

-- Output
%%[8 import({%{EH}EHC.CompilePhase.Output})
%%]

-- EH semantics
%%[8 import(qualified {%{EH}EH.Main} as EHSem)
%%]

-- HI syntax and semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]

-- Core semantics
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(50 codegen corein) import(qualified {%{EH}Core.Check} as Core2ChkSem)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(cpTransformCore)
cpTransformCore :: EHCCompileRunner m => OptimizationScope -> HsName -> EHCompilePhaseT m ()
cpTransformCore optimScope modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Transforming Core ..." Nothing fp
       
         -- transform
       ; let  mbCore     = _ecuMbCore ecu
              trfcoreIn  = emptyTrfCore
                             { trfstMod             	= panicJust "cpTransformCore" mbCore
                             , trfstUniq            	= crsi ^. crsiNextUID
                             , trfstExtra = emptyTrfCoreExtra
                                 { trfcoreECUState		= ecuState ecu
%%[[8
                                 -- , trfcoreIsLamLifted	= False
%%][(50 corein)
                                 -- , trfcoreIsLamLifted	= maybe False Core2ChkSem.isLamLifted_Syn_CodeAGItf $ _ecuMbCoreSemMod ecu
%%]]
%%[[(50 corein)
                                 , trfcoreNotYetTransformed
                                 						= maybe (trfcoreNotYetTransformed emptyTrfCoreExtra) Core2ChkSem.notYetTransformed_Syn_CodeAGItf $ _ecuMbCoreSemMod ecu
%%]]
%%[[50
                                 , trfcoreExpNmOffMp    = crsiExpNmOffMpDbg "cpTransformCore" modNm crsi
%%]]
								 , trfcoreInhLamMp      = crsi ^. crsiCEnv ^. cenvLamMp
                                 }
                             }
              trfcoreOut = trfCore opts optimScope
                                   (crsi ^. crsiCEnv ^. cenvDataGam)
                                   modNm trfcoreIn
       
%%[[(50 corein)
       -- ; liftIO $ putStrLn $ "cpTransformCore trfcoreNotYetTransformed: " ++ show (trfcoreNotYetTransformed $ trfstExtra trfcoreIn)
%%]]
       -- ; cpUpdSI $ (crsiCEnv ^* cenvLamMp) ^= (trfcoreGathLamMp $ trfstExtra trfcoreOut)
       -- put back result: Core
       ; cpUpdCU modNm $! ecuStoreCore (trfstMod trfcoreOut)

         -- put back result: unique counter
       ; cpSetUID (trfstUniq trfcoreOut)

%%[[50
         -- put back result: call info map (lambda arity, ...)
       ; let hii   = ecu ^. ecuHIInfo
             lamMp = HI.hiiLamMp hii
       ; cpUpdCU modNm
           ( ecuStoreHIInfo
               (hii { HI.hiiLamMp = (trfcoreGathLamMp $ trfstExtra trfcoreOut) `Map.union` lamMp
                    })
           )
%%]]   
%%[[99
         -- put back result: additional hidden exports, it should be in a cpFlowXX variant
       ; cpUpdHiddenExports modNm $ zip (Set.toList $ trfcoreExtraExports $ trfstExtra trfcoreOut) (repeat IdOcc_Val)
%%]]

         -- dump intermediate stages, print errors, if any
       ; let (nms,mcs,errs) = unzip3 $ trfstModStages trfcoreOut
             (fc,sf) | CoreOpt_DumpAST `elem` ehcOptCoreOpts opts = (ASTFileContent_ASTText, Cfg.suffixDotlessOutputTextualCoreAST)
                     | otherwise                                  = (ASTFileContent_Text, Cfg.suffixDotlessOutputTextualCore)
       -- ; cpOutputCoreModules CPOutputCoreHow_Text (\n nm -> "-" ++ show optimScope ++ "-" ++ show n ++ "-" ++ nm) Cfg.suffixDotlessOutputTextualCore modNm [ (n,nm) | (n, Just nm) <- zip nms mcs ]
       ; cpOutputSomeModules
           (Just $ opts {ehcOptCoreOpts= CoreOpt_Readable : ehcOptCoreOpts opts})
           astHandler'_Core fc
           (\n nm -> "-" ++ show optimScope ++ "-" ++ show n ++ "-" ++ nm)
           sf modNm
           [ (n,nm) | (n, Just nm) <- zip nms mcs ]
       ; cpSeq $ zipWith (\nm err -> cpSetLimitErrsWhen 5 ("Core errors: " ++ nm) err) nms errs
       }
%%]

%%[(8 javascript) export(cpTransformJavaScript)
cpTransformJavaScript :: EHCCompileRunner m => OptimizationScope -> HsName -> EHCompilePhaseT m ()
cpTransformJavaScript optimScope modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Transforming JavaScript ..." Nothing fp
       
         -- transform
       ; let  mbJavaScript     = _ecuMbJavaScript ecu
              trfjsIn  = emptyTrfJavaScript
                             { trfstMod           = panicJust "cpTransformJavaScript" mbJavaScript
                             , trfstUniq          = crsi ^. crsiNextUID
                             }
              trfjsOut = trfJavaScript opts optimScope modNm trfjsIn
       
         -- put back result: JavaScript
       ; cpUpdCU modNm $! ecuStoreJavaScript (trfstMod trfjsOut)

         -- put back result: unique counter
       ; cpSetUID (trfstUniq trfjsOut)

         -- dump intermediate stages, print errors, if any
       ; let (nms,mcs,errs) = unzip3 $ trfstModStages trfjsOut
       -- ; cpOutputJavaScriptModules ASTFileContent_Text (\n nm -> "-" ++ show n ++ "-" ++ nm) Cfg.suffixJavaScriptLib modNm [ (n,nm) | (n, Just nm) <- zip nms mcs ]
       ; cpOutputSomeModules Nothing astHandler'_JavaScript ASTFileContent_Text (\n nm -> "-" ++ show n ++ "-" ++ nm) Cfg.suffixJavaScriptLib modNm [ (n,nm) | (n, Just nm) <- zip nms mcs ]
       ; cpSeq $ zipWith (\nm err -> cpSetLimitErrsWhen 5 ("JavaScript errors: " ++ nm) err) nms errs
       }
%%]

%%[(8 codegen cmm) export(cpTransformCmm)
cpTransformCmm :: EHCCompileRunner m => OptimizationScope -> HsName -> EHCompilePhaseT m ()
cpTransformCmm optimScope modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Transforming Cmm ..." Nothing fp
       
         -- transform
       ; let  mbCmm     = _ecuMbCmm ecu
              trfcmmIn  = emptyTrfCmm
                             { trfstMod           = panicJust "cpTransformCmm" mbCmm
                             , trfstUniq          = crsi ^. crsiNextUID
                             }
              trfcmmOut = trfCmm opts optimScope modNm trfcmmIn
       
         -- put back result: Cmm
       ; cpUpdCU modNm $! ecuStoreCmm (trfstMod trfcmmOut)

         -- put back result: unique counter
       ; cpSetUID (trfstUniq trfcmmOut)

         -- dump intermediate stages, print errors, if any
       ; let (nms,mcs,errs) = unzip3 $ trfstModStages trfcmmOut
       -- ; cpOutputCmmModules ASTFileContent_Text (\n nm -> "-" ++ show n ++ "-" ++ nm) Cfg.suffixCmmLib modNm [ (n,nm) | (n, Just nm) <- zip nms mcs ]
       ; cpOutputSomeModules Nothing astHandler'_Cmm ASTFileContent_Text (\n nm -> "-" ++ show n ++ "-" ++ nm) Cfg.suffixCmmLib modNm [ (n,nm) | (n, Just nm) <- zip nms mcs ]
       ; cpSeq $ zipWith (\nm err -> cpSetLimitErrsWhen 5 ("Cmm errors: " ++ nm) err) nms errs
       }
%%]

%%[(8 core) export(cpTransformCoreCPS)
cpTransformCoreCPS :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTransformCoreCPS modNm
  = do { cr <- get
       ; let (ecu, crsi, opts, fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Transforming CoreCPS ..." Nothing fp
       ; let mbCoreCPS = _ecuMbCoreCPS ecu
             trfCoreCPSOut = fmap (trfCoreCPS modNm) mbCoreCPS
       ; cpUpdCU modNm $! ecuStoreCoreCPS (fromJust trfCoreCPSOut)
       }
%%]
