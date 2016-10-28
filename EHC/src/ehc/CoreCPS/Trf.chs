%%[(8 core) hs module ({%{EH}CoreCPS.Trf})
%%]
%%[(8 core) import({%{EH}CoreCPS.Trf.GlobUniq}, {%{EH}Base.HsName}, {%{EH}CoreCPS})
%%]
%%[(8 core) export(trfCoreCPS)
trfCoreCPS :: HsName -> CTm -> CTm
trfCoreCPS nm = ctmTrfGlobUniq
%%]
