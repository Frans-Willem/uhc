%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Lua bytecode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 core) module {%{EH}EHC.CompilePhase.CompileLuaBC}
%%]
%%[(8 core) import (Control.Monad.State)
%%]
%%[(8 core) import ({%{EH}EHC.Common})
%%]
%%[(8 core) import ({%{EH}EHC.CompileRun})
%%]
%%[(8 core) import ({%{EH}Base.Target})
%%]

%%[(8 core) export(cpCompileLuaBC)
cpCompileLuaBC :: EHCCompileRunner m => FinalCompileHow -> [HsName] -> HsName -> EHCompilePhaseT m ()
cpCompileLuaBC how othModNmL modNm
  = do { cr <- get
       ; let (ecu,_,opts,fp) = crBaseInfo modNm cr
             fpCl c = mkOutputFPath opts c (fpathSetBase (show c) fp) "lc"
       ; when (targetIsLuaBC (ehcOptTarget opts))
              (do { cpMsg modNm VerboseALot $ "Emit Lua BC: " ++ (fpathToStr fp)
                  }
              )
       }
%%]
