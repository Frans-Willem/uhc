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
%%[(8 core) import ({%{EH}Base.Common})
%%]
%%[(8 core) import ({%{EH}CodeGen.Bits})
%%]
%%[(8 core) import ({%{EH}LuaBC.ToBinary})
%%]
%%[(8 core) import({%{EH}EHC.CompileUnit})
%%]

%%[(8 core) export(cpCompileLuaBC)
cpCompileLuaBC :: EHCCompileRunner m => FinalCompileHow -> [HsName] -> HsName -> EHCompilePhaseT m ()
cpCompileLuaBC how othModNmL modNm
  = do { cr <- get
       ; let (ecu,_,opts,fp) = crBaseInfo modNm cr
             fpChunk = mkOutputFPath opts modNm (fpathSetBase (show modNm) fp) "lc"
       ; when (targetIsLuaBC (ehcOptTarget opts))
              (do { cpMsg modNm VerboseALot $ "Emit Lua BC: " ++ (fpathToStr fpChunk)
                  ; liftIO $ fpathEnsureExists fpChunk
                  ; let chunk = fromJust $ _ecuMbLuaBC ecu
                        binaryChunk = chunk2binary chunk
                  ; liftIO $ writeBinaryToFile (bytesToString binaryChunk) fpChunk
                  }
              )
       }
%%]
