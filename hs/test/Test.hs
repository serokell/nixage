import Universum

import qualified Data.ByteString.Lazy as BS
import Data.Tuple.Select (sel1, sel2)
import System.FilePath (addExtension, dropExtension, takeBaseName, (</>))
import qualified Test.SmallCheck.Series as TSS
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

import Nixage.Convert.FromYaml (decodeFromYaml)
import Nixage.Convert.Nix (projectNativeToPrettyNix)
import Nixage.Convert.Stack (projectNativeToStackFiles)
import Nixage.Project (ProjectNative)

main :: IO ()
main = do
    fromProjectYaml <- fromProjectYamlIO
    defaultMain $ testGroup "Nixage"
        [ fromProjectYaml
        , projectNativeSeries
        ]

inputDir, goldenDir :: FilePath
inputDir = "./test/input"
goldenDir = "./test/golden"

fromProjectYamlIO :: IO TestTree
fromProjectYamlIO = do
    projectYamlPaths <- findByExtension [".project-yaml"] inputDir
    return $ testGroup "From yaml to stack and nix" $ concat
        [ goldenTests testNameTemplate getProjectNative
        | projectYamlPath <- projectYamlPaths
        , let getProjectNative = decodeFromYaml projectYamlPath
              testNameTemplate = takeBaseName $ dropExtension projectYamlPath
        ]


projectNativeSeries :: TestTree
projectNativeSeries = testGroup "SmallCheck's series for ProjectNative" $ concat
    [ goldenTests testNameTemplate (return projectNative)
      | (i, projectNative) <- take nbrOfTests $ zip [0::Int ..] projectNatives
      , let testNameTemplate = "project-native-series-" <> show i
    ]
  where
   projectNatives = TSS.list smallCheckDepth $ TSS.series @_ @ProjectNative
   smallCheckDepth = 5
   nbrOfTests = 10


goldenTests :: String            -- ^ Golden file name without any exstension
            -> IO ProjectNative  -- ^ Action for creating ProjectNative
            -> [TestTree]
goldenTests testNameTemplate action =
    [ goldenVsString
        (testNameTemplate <> "-stack")
        stackYamlGoldenPath -- golden file path
        (BS.fromStrict . sel1 . toStackFiles <$> action )  -- action whose result is tested

    , goldenVsString
        (testNameTemplate <> "-snapshot")
        snapshotYamlGoldenPath
        (BS.fromStrict . sel2 . toStackFiles <$> action )

    , goldenVsString
        (testNameTemplate <> "-nix")
        nixGoldenPath
        (show . projectNativeToPrettyNix <$> action )
    ]
  where
    [stackYamlGoldenPath, snapshotYamlGoldenPath, nixGoldenPath] =
        (</>) goldenDir . addExtension testNameTemplate
        <$> ["stack-yaml-golden", "snapshot-yaml-golden", "nix-golden"]
    toStackFiles =
        projectNativeToStackFiles "test-snapshot.yaml" "test-stack-shell.nix"
                                  "test-stack-shell-source.nix"
