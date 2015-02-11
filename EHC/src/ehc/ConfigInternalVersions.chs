%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Configuration, internal versions acting as a weak timestamp to distinguish different formats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 module {%{EH}ConfigInternalVersions}
%%]

%%[50 import (Data.Word, Data.Bits)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Versions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 hs export(InternalVersionCombined)
-- | Encoding of internal version
type InternalVersion 			= Word64
type InternalVersionCombined 	= Word64

mkInternalVersion :: Int -> InternalVersion
mkInternalVersion = fromIntegral
%%]

%%[50 hs export(internalVersionCombined)
internalVersionCombined :: InternalVersionCombined
internalVersionCombined =
      internalVersionHI
  .|. (internalVersionCore 		`shiftL` 8)
  .|. (internalVersionCoreRun 	`shiftL` 16)
%%]

%%[50 hs export(internalVersionHI, internalVersionCore, internalVersionCoreRun)
-- | For binary/serialized HI .hi files and all data stored there
internalVersionHI = mkInternalVersion 1

-- | For binary/serialized Core .cr/.bcr/.tcr etc files
internalVersionCore = mkInternalVersion 14

-- | For binary/serialized CoreRun .crr/.bcrr./tcrr etc files
internalVersionCoreRun = mkInternalVersion 4
%%]

