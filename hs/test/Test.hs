import Universum

import qualified Data.ByteString.Lazy as BS
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

import Nixage.Convert.FromYaml (decodeFromYaml)
import Nixage.Convert.Nix (projectNativeToPrettyNix)
import Nixage.Convert.Stack (projectNativeToStackFiles)

main :: IO ()
main = defaultMain =<< fromProjectYaml

fromProjectYaml :: IO TestTree
fromProjectYaml = do
    projectYamlPaths <- findByExtension [".project-yaml"] "."
    return $ testGroup "From yaml to stack and nix" $ concat
        [ [ goldenVsString
              (testName <> "-stack")
              stackYamlGoldenPath -- golden file path
              (BS.fromStrict . fst . toStackFiles <$> getProjectNative)  -- action whose result is tested

          , goldenVsString
              (testName <> "-snapshot")
              snapshotYamlGoldenPath
              (BS.fromStrict . snd . toStackFiles <$> getProjectNative)

          , goldenVsString
              (testName <> "-nix")
              nixGoldenPath
              (show . projectNativeToPrettyNix <$> getProjectNative)
          ]

        | projectYamlPath <- projectYamlPaths
        , let testName  = takeBaseName projectYamlPath
              stackYamlGoldenPath = replaceExtension projectYamlPath ".stack-yaml-golden"
              snapshotYamlGoldenPath = replaceExtension projectYamlPath ".snapshot-yaml-golden"
              nixGoldenPath = replaceExtension projectYamlPath ".nix-golden"
              getProjectNative = decodeFromYaml projectYamlPath
        ]
  where
    toStackFiles = projectNativeToStackFiles "test-snapshot.yaml" "test-stack-shell.nix"
