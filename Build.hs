{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes

import Distribution.Simple.GHC (ghcPlatformAndVersionString)
import Distribution.System (Platform (..), buildArch, buildOS)
import Distribution.Types.Version (mkVersion)

import Clash.Driver.Types (Manifest (..))
import Data.List.Split (splitOn)

-- A string like "8.10.2"
newtype GhcVersion = GhcVersion ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult GhcVersion = String

topModule = "Euphrates"
topEntity = "euphrates"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  want ["_build/euphrates.bit"]

  getGhcVersion <- addOracle $ \(GhcVersion _) ->
    fromStdout <$> cmd "ghc --numeric-version" :: Action String

  "_build/euphrates.bit" %> \out -> do
    need ["_build/ulx3s_out.config"]
    cmd_ "ecppack" "_build/ulx3s_out.config" out

  "_build/ulx3s_out.config" %> \out -> do
    need ["_build/euphrates.json", "ulx3s_v20.lpf"]
    cmd_ "nextpnr-ecp5" $ unwords
      [ "--85k"
      , "--json _build/euphrates.json"
      , "--lpf ulx3s_v20.lpf"
      , "--textcfg", out
      ]

  "_build/euphrates.json" %> \_ -> do
    need ["_build/euphrates.ys"]
    cmd_ "yosys _build/euphrates.ys"

  "_build/euphrates.ys" %> \out -> do
    let manifestDir = "_build/verilog" </> topModule </> topEntity
    manifest <- read <$> readFile' (manifestDir </> topEntity <.> "manifest") :: Action Manifest
    let verilogSources = map (manifestDir </>) (filter (".v" `isExtensionOf`) (fileNames manifest))
    writeFileLines out
      [ "read_verilog " <> unwords verilogSources
      , "synth_ecp5 -top euphrates -noccu2 -nomux -nodram -json _build/euphrates.json"
      ]

  "_build/verilog/Euphrates/euphrates/euphrates.manifest" %> \_ -> do
    versionString <- getGhcVersion $ GhcVersion ()
    let version = mkVersion $ map read $ splitOn "." versionString
    let platform = Platform buildArch buildOS
    need [".ghc.environment" <.> ghcPlatformAndVersionString platform version]
    cmd_ "cabal run clash -- -fclash-hdldir _build --verilog Euphrates.Top"

  ".ghc.environment.*" %> \_ -> do
    need ["euphrates.cabal"]
    cmd_ "cabal build --write-ghc-environment-files=always"

  phony "clean" $ do
    putInfo "Cleaning files in _build and GHC environment file(s)"
    liftIO $ removeFiles "." [".ghc.environment.*"]
    removeFilesAfter "_build" ["//*"]
