%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Javascript
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

JavaScript compilation

%%[(8 codegen javascript) module {%{EH}EHC.CompilePhase.CompileJavaScript}
%%]

%%[(8 codegen javascript) import(System.Directory, Data.List(intercalate), Data.Either, System.Exit)
%%]
%%[(8 codegen javascript) import(qualified Data.Map as Map)
%%]
%%[(8 codegen javascript) import(UHC.Util.Lens)
%%]
%%[(8 codegen javascript) import(Control.Monad.State)
%%]

-- AST handling
%%[(8 codegen javascript) import({%{EH}EHC.ASTHandler.Instances})
%%]

-- general imports
%%[(8 codegen javascript) import({%{EH}EHC.Common})
%%]
%%[(8 codegen javascript) import({%{EH}EHC.CompileUnit})
%%]
%%[(8 codegen javascript) import({%{EH}EHC.CompileRun})
%%]
%%[(8 codegen javascript) import({%{EH}EHC.CompilePhase.Output})
%%]

%%[(8 codegen javascript) import(qualified {%{EH}Config} as Cfg)
%%]
%%[(8 codegen javascript) import({%{EH}EHC.Environment})
%%]
%%[(8 codegen javascript) import({%{EH}Base.Target})
%%]

%%[(8 codegen javascript) import({%{EH}Core.ToJavaScript})
%%]
%%[(8 codegen javascript) import({%{EH}CodeGen.Bits},{%{EH}JavaScript.Pretty})
%%]
%%[(99 codegen javascript) import({%{EH}Base.FileSearchLocation},{%{EH}Base.PackageDatabase})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: JavaScript linking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen javascript) export(cpJavaScript)
cpJavaScript :: EHCCompileRunner m => FilePath -> [FilePath] -> EHCompilePhaseT m ()
cpJavaScript archive files
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
             cmd = mkShellCmd $ [Cfg.shellCmdCat] ++ files -- ++ [">", archive]
       ; when (ehcOptVerbosity opts >= VerboseALot) (liftIO $ putStrLn $ showShellCmd cmd ++ " > " ++ archive)
       ; cpSystem' (Just archive) cmd 
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: JavaScript compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen javascript) export(cpCompileJavaScript)
cpCompileJavaScript :: EHCCompileRunner m => FinalCompileHow -> [HsName] -> HsName -> EHCompilePhaseT m ()
cpCompileJavaScript how othModNmL modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
%%[[8
              odirStrip       = id
              outputToOtherLoc= False
%%][99
              odirStrip       = maybe id (\ofp fp -> (maybe id fpathUnPrependDir $ fpathMbDir ofp) fp) $ ehcOptMbOutputFile opts
              outputToOtherLoc= isJust (ehcOptMbOutputFile opts) || isJust (ehcOptOutputDir opts)
%%]]
              canonicalize    = if outputToOtherLoc then canonicalizePath else return
              optsNoOdir      = opts
%%[[99
                                  {ehcOptOutputDir = Nothing}
%%]]
              mbJs            = _ecuMbJavaScript ecu
              fpOOpts o m f   = _asthdlrMkOutputFPath astHandler'_JavaScript o m f Cfg.suffixJavaScriptLib
              fpO m f         = fpOOpts opts m f
              fpExecOpts o    = mkPerExecOutputFPath o modNm fp (Just ("js", True))
              fpExec          = fpExecOpts opts
              fpHtml          = mkPerExecOutputFPath opts modNm fp (Just ("html", False))
              variant         = Cfg.installVariant opts

       ; when (isJust mbJs && ehcOptEmitJavaScript opts)
              (do { when (ehcOptVerbosity opts >= VerboseDebug)
                         (do { liftIO $ putStrLn $ "fpExec: " ++ fpathToStr fpExec
                             -- ; liftIO $ putStrLn $ "fpExec (canon): " ++ fileNmExec
                             ; liftIO $ putStrLn $ show (ehcOptImportFileLocPath opts)
                             ; liftIO $ putStrLn $ "module dependencies:" ++ intercalate "," (jsModDeps (fromJust mbJs))
                             })

                  ; let fpDeps  = map fpathFromStr (jsModDeps (fromJust mbJs))
                  ; let searchPath = ehcOptImportFileLocPath opts

                  ; jsDepsFound <- jsDepsToFPaths searchPath fpDeps
                  
                  ; let someJsDepsNotFound = either (const True) (const False)

                  ; when (someJsDepsNotFound jsDepsFound) 
                         (do { let Left notFound = jsDepsFound
                             ; err $ "Could not find external js dependencies: " ++ intercalate "," (map fpathToStr notFound)
                             })

                  ; let Right jsDeps = jsDepsFound

                  -- ; fpM <- cpOutputJavaScript ASTFileContent_Text "" modNm
                  ; fpM <- fmap (panicJust "cpCompileJavaScript.cpOutputSomeModule") $
                      cpOutputSomeModule (^. ecuJavaScript) astHandler'_JavaScript ASTFileContent_Text "" Cfg.suffixJavaScriptLib modNm
                  
                  -- ; fileNmExec <- liftIO $ canonicalize $ fpathToStr fpExec 

                  ; case how of
                      FinalCompile_Exec
%%[[50
                        | ehcOptWholeProgOptimizationScope opts
                        -> do { cpJavaScript (fpathToStr fpExec) (rts ++ [fpathToStr fpM])
                              ; fileNmExec <- liftIO $ canonicalize $ fpathToStr fpExec 
                              ; mkHtml fpHtml ((map fpathToStr jsDeps) ++ [fileNmExec])
                              }
%%]]
                        | otherwise
                        -> do { cpJavaScript (fpathToStr fpExec) [fpathToStr fpM]
                              ; fileNmExec <- liftIO $ canonicalize $ fpathToStr fpExec 
                              ; othModFileNmL <- liftIO $ forM [ fpathToStr $ fpO m fp | m <- othModNmL2, let (_,_,_,fp) = crBaseInfo m cr ] canonicalize
                              ; mkHtml fpHtml $
                                  ( map fpathToStr jsDeps )
                                  ++ rts
%%[[99
                                  ++ concat
                                       [ [ mkpgkl k m | m <- ms ]
                                       | (k,(_,ms)) <- Map.toList pkgKeyDirModsMp
                                       ]
%%]]
                                  ++ othModFileNmL -- [ fpathToStr $ odirStrip $ fpO m fp | m <- othModNmL2, let (_,_,_,fp) = crBaseInfo m cr ] 
                                  ++ [ fileNmExec ] -- [ fpathToStr $ odirStrip fpExec ]
                              }
                        where rts = map (Cfg.mkInstalledRts opts Cfg.mkJavaScriptLibFilename Cfg.INST_LIB (Cfg.installVariant opts)) Cfg.libnamesRts
                              (pkgKeyDirL,othModNmL2)
%%[[8
                                = ([], othModNmL)
%%][99
                                = crPartitionIntoPkgAndOthers cr othModNmL
                              (_,pkgKeyDirModsMp) = pkgPartInclDirs opts pkgKeyDirL
%%]]
%%[[8
                              oth = []
%%][50
                              oth | ehcOptWholeProgOptimizationScope opts = []
                                  | otherwise                             = [ fpO m fp | m <- othModNmL2, let (_,_,_,fp) = crBaseInfo m cr ]
%%]]
%%[[99
                              mkpgkl l m = Cfg.mkInstallFilePrefix opts Cfg.INST_LIB_PKG2 variant (showPkgKey l)
                                               ++ "/" ++ mkInternalPkgFileBase l (Cfg.installVariant opts) (ehcOptTarget opts) (ehcOptTargetFlavor opts)
                                               ++ "/" ++ (fpathToStr $ fpOOpts optsNoOdir m $ mkFPath m)
%%]]

                      _ -> return ()
                  }
              )
       }
  where mkHtml fpHtml jsL
          = liftIO $ putPPFPath fpHtml (ppHtml) 1000
          where scr x = "<script type=\"text/javascript\" src=\"" >|< x >|< "\"></script>"
                ppHtml
                  = "<!DOCTYPE html><html><head><title>" >|< modNm >|< "</title>"
                    >-< vlist (map scr jsL)
                    >-< "</head>"
                    >-< "<body>"
                    >-< "</body>"
                    >-< "</html>"

        findJsDep :: EHCCompileRunner m => FileLocPath -> FPath -> EHCompilePhaseT m (Maybe FPath)
        findJsDep searchPath dep = liftIO $ searchPathForReadableFile (map filelocDir searchPath) [Just "js"] dep

        jsDepsToFPaths :: EHCCompileRunner m => FileLocPath -> [FPath] -> EHCompilePhaseT m (Either [FPath] [FPath])
        jsDepsToFPaths searchPath deps = do
          paths <- mapM (\dep -> do {
                    ; depFound <- findJsDep searchPath dep
                    ; maybe (return $ Left dep) (return . Right) depFound
                    }) deps

          let allLeft          = lefts paths
              hasUnfoundJsDeps = not $ null $ allLeft

          if hasUnfoundJsDeps
            then return $ Left allLeft
            else return $ Right $ rights paths

        err :: EHCCompileRunner m => String -> EHCompilePhaseT m ()
        err x = do 
          liftIO $ hPutStrLn stderr ("error: " ++ x)
          liftIO $ exitFailure
%%]



