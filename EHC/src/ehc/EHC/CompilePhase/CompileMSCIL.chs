%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile MSCIL bytecode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 core) module {%{EH}EHC.CompilePhase.CompileMSCIL}
%%]
%%[(8 core) import (Control.Monad.State)
%%]
%%[(8 core) import ({%{EH}EHC.Common})
%%]
%%[(8 core) import ({%{EH}EHC.CompileRun})
%%]
%%[(8 core) import ({%{EH}Base.Target})
%%]
%%[(8 core) import ({%{EH}Base.Common})
%%]
%%[(8 core) import ({%{EH}CodeGen.Bits})
%%]
%%[(8 core) import ({%{EH}LuaBC.ToBinary})
%%]
%%[(8 core) import({%{EH}EHC.CompileUnit})
%%]
%%[(8 core) import({%{EH}EHC.CompilePhase.Output})
%%]

%%[(8 core) export(cpCompileMSCIL)
cpCompileMSCIL :: EHCCompileRunner m => FinalCompileHow -> [HsName] -> HsName -> EHCompilePhaseT m ()
cpCompileMSCIL how othModNmL modNm
  = do { cr <- get
       ; let (ecu,_,opts,fp) = crBaseInfo modNm cr
             fpChunk = mkOutputFPath opts modNm (fpathSetBase (show modNm) fp) "il"
       ; when (targetIsMSCIL (ehcOptTarget opts))
              (do { cpMsg modNm VerboseALot $ "Emit MSCIL: " ++ (fpathToStr fpChunk)
                  ; void $ cpOutputMSCIL ASTFileContent_Text "" modNm
                  ; let compileCommand = mkShellCmd ["ilasm", fpathToStr fpChunk ]
                  ; cpSystem compileCommand
                  }
              )
       }
%%]
