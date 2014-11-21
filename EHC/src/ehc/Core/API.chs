%%[(8 core)

-- | Core Public API (provisional, to be refactored)
--
-- Intended for constructing basic Core Programs. Use the binary serialization from `UHC.Util.Binary`
-- to produce a core file, which can be compiled by UHC.
--
-- Restrictions:
--  - Extendable data types are not supported
--  - Generated code is not (type-)checked, might cause runtime crashes
--  - Core parsing/Pretty printing is incomplete and might be partially broken.
-- Invariants:
-- - Constructor applications (mkCon) always have to be fully saturated. (Should we handle this internally?)
-- - Haskell constructor names must be unambigous per module (mkHSCTag)

module %%@{%{EH}%%}Core.API
  (
  {-
  -- Opts
    EHCOpts
  , defaultEHCOpts
  -}

  {-
  -- Base.Common
  , CTag
  -- Base.HsName
  , HsName
  -}

  -- * Names
  -- | Names in UHC have to be of the form P1.P2....Pn.Ident . All names
  -- in module M must have the form M.Ident . Datatype and constructor names
  -- have to start with an uppercase letter, functions with a lowercase letter.
    mkUniqueHsName
  , mkHsName
  , mkHsName1
  , addHsNamePrefix

  -- * Core AST
  -- | The datatypes making up a Core program.
  , CModule
  , CImport
  , CDeclMeta
  , CDataCon
  , CExpr
  , CBind
  , CAlt
  , CPat
  , CPatFld

  -- * Construction functions
  -- ** Constants
  , acoreUnit
  , acoreInt

%%[[97
  , acoreBuiltinInteger -- TODO acoreInt2 or acoreBuiltinInteger ?
%%]]
  , acoreBuiltinString
  , acoreBuiltinError
  , acoreBuiltinUndefined

  -- ** Variables
  , acoreVar

  -- ** Let Bindings
  , acoreLet1Plain
  , acoreLet1Strict
  , acoreLetRec

  -- ** Abstraction
  , acoreLam

  -- ** Application
  , acoreApp

  -- ** Binds/Bounds
  , acoreBind1
  , acoreBind1Nm1

  -- ** Constructor tags
  , makeCTag
  , destructCTag

  , ctagUnit
  , ctagTup
  , ctagTrue
  , ctagFalse
  , ctagCons
  , ctagNil

  -- ** Case
  -- | Scrutinizes an expression and executes the appropriate alternative.
  -- The scrutinee of a case statement is required to be in WHNF (weak head normal form).
  , acoreCaseDflt

  , acoreAlt
  , acorePatCon
  , acorePatRestEmpty
  , acorePatFldBind


  -- ** Datatypes
  , acoreTagTup


  -- ** Module
  , makeModule
  , makeImport
  , makeMetaData
  , makeMetaDataCon
  , makeMetaDataConFromCTag

  -- * Utilities
  , makeMain
  , ppCModule
  , pCExpr
  , coreScanOpts
  
  -- * Re-exports (or not???)
  , module %%@{%{EH}%%}Base.API
  )
  where

import qualified Data.Map as M
import Data.List
import Data.Ord

import %%@{%{EH}%%}AbstractCore hiding (acoreCaseDflt)
import qualified %%@{%{EH}%%}AbstractCore as AC
import %%@{%{EH}%%}Base.API
import %%@{%{EH}%%}Base.Common
import %%@{%{EH}%%}Base.HsName
import %%@{%{EH}%%}Core hiding (acoreCaseDflt)
import %%@{%{EH}%%}Core.Pretty
import %%@{%{EH}%%}Core.Parser
import %%@{%{EH}%%}Scanner.Common
import %%@{%{EH}%%}Opts
import %%@{%{EH}%%}CodeGen.Tag

-- | Creates a new Core name. All names generated with this function live in
-- the "Core API" namespace and will not collide with names in other namespaces.
-- Names in the "Core API" namespace cannot be called from Haskell code.
--
-- Use this function to create names used only inside Core code generated by your own Compiler,
-- e.g. module-scoped or local functions.
mkUniqueHsName :: String    -- ^ Name prefix. Used to distinguish names generated by different API consumers,
                            -- but may also be used to differentiate between different varieties by one API consumer.
                            -- Use reverse-dns notation if possible, e.g. "nl.uu.agda.identOfVarietyA"
    -> [String]             -- ^ The module prefix.
    -> String               -- ^ The name to make unique.
    -> HsName
-- UHC expects names to be of the _Modf variety. If _Base/hsnFromString is used
-- instead things start to break, e.g. calling functions defined in other packages.
mkUniqueHsName prefix = hsnMkModf1 (M.singleton HsNameUniqifier_CoreAPI [HsNameUnique_String prefix])

-- | Creates a new Core name. The generated name lives in the default namespace,
-- hence may clash with Haskell-defined names.
mkHsName :: [String]    -- ^ The module prefix.
    -> String           -- ^ The local name of the identifier.
    -> HsName
mkHsName = hsnMkModf1 M.empty

-- | Creates a new Core name. The generated name lives in the default namespace,
-- hence may clash with Haskell-defined names.
mkHsName1 :: String
    -> HsName
mkHsName1 nm = mkHsName (init xs) (last xs)
  -- TODO there is probably something like that somewhere in UHC?
  where xs = splitBy '.' nm
        splitBy :: Eq a => a -> [a] -> [[a]]
        splitBy sep = (foldr (\x (a1:as) -> if x == sep then ([]:a1:as) else ((x:a1):as)) [[]])

-- | Adds an additional prefix to a 'HsName'. This can be used to derive a new
-- unique name from an existing name.
addHsNamePrefix :: String -> HsName -> HsName
addHsNamePrefix prefix name = hsnUniqifyStr HsNameUniqifier_CoreAPI prefix name

-- | Local helper function. Converts string names to HsNames.
hsnMkModf1 :: HsNameUniqifierMp -> [String] -> String -> HsName
-- UHC expects names to be of the _Modf variety. If _Base/hsnFromString is used
-- instead things start to break, e.g. calling functions defined in other packages.
hsnMkModf1 uniq mods nm = hsnMkModf mods (hsnFromString nm) uniq

-- TODO how should we handle the type?
-- | Creates the unit expresssion.
acoreUnit :: EHCOpts -> CExpr
acoreUnit _ = acoreTup []

-- | Creates a constructor tag.
makeCTag :: HsName  -- ^ Fully qualified Datatype name.
    -> HsName       -- ^ Fully qualified Constructor name.
    -> Int          -- ^ Tag number.
    -> Int          -- ^ Arity.
    -> CTag
makeCTag tyNm conNm tg ar = CTag tyNm conNm tg ar (-1)

destructCTag :: a -- ^ Algebra for record/tuple case.
    -> (HsName -> HsName -> Int -> Int -> a)    -- ^ Algebra for datatype case. Order of arguments is the same as in 'makeCTag'.
    -> CTag
    -> a
destructCTag arec _ CTagRec = arec
destructCTag _ adat (CTag {ctagTyNm = ty, ctagNm = nm, ctagTag' = tag, ctagArity = ar}) = adat ty nm tag ar

-- | `CTag` for unit values ('()' in haskell).
ctagUnit :: CTag
ctagUnit = ctagTup

-- | `CTag` of tuple/records.
ctagTup :: CTag
ctagTup = CTagRec

-- TODO verify that this sorting is always correct (see also AbstractCore/Utils.chs)
-- | A Case expression, possibly with a default value.
acoreCaseDflt  :: AbstractCore  e m b bound boundmeta bcat mbind t p pr pf a
        => e        -- ^ The scrutinee. Required to be in WHNF.
        -> [a]      -- ^ The alternatives.
        -> Maybe e  -- ^ The default value. (TODO what is the behaviour if it is Nothing?)
        -> e
acoreCaseDflt e as def =
  AC.acoreCaseDflt e (sortBy (comparing (getTag . fst . acoreUnAlt)) as) def
  where -- gets the for constructors, or returns 0 if this is not a constructor pattern
        -- TODO is this always safe?
        getTag t = case acorePatMbCon t of
                        Just (tag, _, _) -> ctagTag tag
                        Nothing          -> 0

-- | Creates a module.
makeModule :: HsName    -- ^ The name of the module.
    -> [CImport]        -- ^ The imports (only direct imports, not transitive ones).
    -> [CDeclMeta]      -- ^ The meta information.
    -> CExpr            -- ^ The body of the module.
    -> CModule
makeModule = CModule_Mod

-- | Creates an import.
makeImport :: HsName -- ^ The module to import.
    -> CImport
makeImport = CImport_Import

-- | Creates the metadata for one datatype.
makeMetaData :: HsName  -- ^ The name of the dataype.
    -> [CDataCon]       -- ^ The constructors of the dataype.
    -> CDeclMeta
makeMetaData = CDeclMeta_Data

-- | Creates the metadata for one constructor.
makeMetaDataCon :: HsName   -- ^ The fully qualified name of the constructor.
    -> Int                  -- ^ The tag of this constructor.
    -> Int                  -- ^ The arity of this constructor.
    -> CDataCon
makeMetaDataCon = CDataCon_Con

makeMetaDataConFromCTag :: CTag -- ^ CTag to export.
    -> Maybe CDataCon   -- ^ The constructor description. Nothing if it is a record/tuple constructor.
makeMetaDataConFromCTag = destructCTag Nothing (\_ b c d -> Just $ makeMetaDataCon b c d)

-- | Creates the main entry point, calling the given function when run. The given
-- function to call has to be in scope (either define it in the same module,
-- or import it).
-- In addition, the module "UHC.Run" has to be imported!
makeMain :: HsName       -- ^ The function containing the user code to call.
    -> CExpr
makeMain main = mainEhc
  where mainEhc = acoreLet1Plain mainNm
            (mainWrap $ acoreVar main)
            (acoreVar mainNm)
        mainNm = hsnMain
%%[[8
        mainWrap = id
%%][99
        mainWrap = \m -> acoreApp (acoreVar hsnEhcRunMain) [m]
%%]]


%%]
