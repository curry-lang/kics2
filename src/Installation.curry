--------------------------------------------------------------------------
--- Definition of some global installation infos.
--- The current installation infos are taken from the
--- file `ROOT/runtime/Installation.hs` which is automatically generated.
---
--- @author  Michael Hanus, Bjoern Peemoeller, Fabian Skrlac
--- @version February 2017
--- --------------------------------------------------------------------------

module Installation where

compilerName :: String
compilerName external

installDir :: String
installDir external

majorVersion :: Int
majorVersion external

minorVersion :: Int
minorVersion external

revisionVersion :: Int
revisionVersion external

buildVersion :: Int
buildVersion external

compilerDate :: String
compilerDate external

installDate :: String
installDate external

runtime :: String
runtime external

runtimeMajor :: Int
runtimeMajor external

runtimeMinor :: Int
runtimeMinor external

ghcExec :: String
ghcExec external

-- GHC options for using local libraries and not cabal packages:
ghcLocalOptions :: String
ghcLocalOptions external

ghcOptions :: String
ghcOptions external

ghcOptimizations :: String
ghcOptimizations external

withProfiling :: Bool
withProfiling external
