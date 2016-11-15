%%[(8 core) hs module ({%{EH}CoreCPS.Trf})
%%]
%%[(8 core) import({%{EH}CoreCPS.Trf.GlobUniq}, {%{EH}CoreCPS.Trf.DropUnused}, {%{EH}Base.HsName}, {%{EH}CoreCPS}, {%{EH}CoreCPS.Trf.Unrec}, {%{EH}CoreCPS.Trf.CleanIds}, {%{EH}CoreCPS.Trf.LiftLet})
%%]
%%[(8 core) export(trfCoreCPS)
trfs :: [(CTm -> CTm, String)]
trfs =
  [ (ctmTrfGlobUniq, "globuniq")
  , (ctmTrfLiftLet, "liftlet")
  , (ctmTrfUnrec, "unrec")
  , (ctmTrfDropUnused, "dropunused")
  , (ctmTrfCleanIds, "cleanids")
  ]
trfCoreCPS :: HsName -> CTm -> ([(String,CTm)],CTm)
trfCoreCPS nm initial
  = (zip names results, last results)
  where
    results = scanl (\ctm trf -> trf ctm) initial (map fst trfs)
    names = "initial" : (map snd trfs)
    
%%]
