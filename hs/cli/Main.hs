import Universum

import Data.Yaml (decodeFileEither, encodeFile)
import Options.Applicative ( Parser, info, fullDesc, progDesc, header
                           , execParser, hsubparser, command
                           , forwardOptions, strArgument, ParserInfo, Mod
                           , CommandFields, optional, long, metavar, value
                           , option, auto, helper)
import System.IO.Temp (withSystemTempFile, withTempFile)
import System.Process (waitForProcess, createProcess, delegate_ctlc, proc)

import Nixage.Project.Yaml (ProjectYaml, projectYamlToProjectNative)
import Nixage.Project.Types (NixageError(..))
import Nixage.Convert.Stack (createStackFiles, projectNativeToStackConfig, StackConfig)

main :: IO ()
main = execParser (info nixageP infoMod) >>= \case
    StackCmd args -> stackAction args
    ConvertCmd convertArgs -> convertAction convertArgs
  where
    infoMod = header "Nixage"
           <> progDesc "Build Haskell packages with Nix and Stackage"
           <> fullDesc


-- | * Nixage cli command types

data NixageCmd =
      StackCmd StackArgs
    | ConvertCmd ConvertArgs

type StackArgs = [Text]

data ConvertArgs = ConvertArgs
    { caInFormat :: InFormat            -- ^ Not optional (default in parser)
    , caOutFromat :: OutFormat
    , caInOutPath :: Maybe (Text, Text) -- ^ Optional (default from 'inFormat')
    } deriving (Show)

data InFormat = YamlInFormat  deriving (Read, Show)
data OutFormat = StackOutFormat deriving (Read, Show)

defaultInPath :: InFormat -> Text
defaultInPath YamlInFormat = "project.yaml"

defaultOutPath :: OutFormat -> Text
defaultOutPath StackOutFormat = "stack.yaml"

-- | * Nixage cli command parsers

nixageP :: Parser NixageCmd
nixageP = hsubparser $ mconcat nixageCmds

nixageCmds :: [Mod CommandFields NixageCmd]
nixageCmds =
    [ command "stack" $ info (StackCmd <$> stackP) $
           progDesc "Run stack on stack.yaml generated from project.yaml"
        <> forwardOptions
    , command "convert" $ info (ConvertCmd <$> convertP) $
           progDesc "Convert between input formats (Yaml, Stack) "
    ]

-- | Parse all arguments after 'stack' command as raw [Text]
stackP :: Parser StackArgs
stackP = many $ strArgument mempty

convertP :: Parser ConvertArgs
convertP = ConvertArgs
       <$> option auto (long "from" <> metavar "in-format" <> value YamlInFormat)
       <*> option auto (long "to" <> metavar "out-format")
       <*> inOutPathP
  where
      inOutPathP :: Parser (Maybe (Text, Text))
      inOutPathP = (,)
               <$> optional (strArgument $ metavar "in-path")
               <*> optional (strArgument $ metavar "out-path") <&> \case
               (Just inPath, Just outPath) -> Just (inPath, outPath)
               _ ->  Nothing


-- | * Nixage command actions

-- | read ProjectYaml from project.yaml
readProjectYaml :: (MonadIO m, MonadThrow m) => FilePath -> m ProjectYaml
readProjectYaml projectYamlPath =
    liftIO (decodeFileEither projectYamlPath) >>= \case
      Left err -> throwM $ YamlDecodingError (show err)
      Right projectYaml -> return projectYaml


-- | Write stack and snapshot yaml files
writeStackConfig :: (MonadIO m, MonadThrow m)
                 => FilePath     -- ^ Snapshot yaml path
                 -> FilePath     -- ^ Stack yaml path
                 -> StackConfig
                 -> m ()
writeStackConfig snapshotPath stackPath stackConfig = do
   let (snapshot, stack) = createStackFiles stackConfig snapshotPath
   liftIO $ do
       encodeFile snapshotPath snapshot
       encodeFile stackPath stack

-- | Create temporary '[standard-tmp-dir]/nixage-snapshot[rand-num].yaml'
-- and './nixage-stack[rand-num].yaml', and run 'stack' on them.
stackAction :: (MonadIO m, MonadThrow m, MonadMask m) => StackArgs-> m ()
stackAction args = do
    projectYaml <- readProjectYaml "project.yaml"
    let projectNative = projectYamlToProjectNative projectYaml
    let stackConfig = projectNativeToStackConfig projectNative
    withSystemTempFile "nixage-stack-snapshot.yaml" $ \snapshotPath _ ->
      withTempFile "." "nixage-stack.yaml" $ \stackPath _ -> do
        writeStackConfig snapshotPath stackPath stackConfig
        let  args' = ["--stack-yaml", toText stackPath] <> args
        liftIO $ do
            (_,_,_,handle) <- createProcess $
                (proc "stack" (toString <$> args')) { delegate_ctlc = True }
            void $ waitForProcess handle

convertAction :: (MonadIO m, MonadThrow m) => ConvertArgs -> m ()
convertAction (ConvertArgs inFormat outFormat mInOutPath) = do
    let (inPath, outPath) =
          fromMaybe (defaultInPath inFormat, defaultOutPath outFormat) mInOutPath
    projectNative <- case inFormat of
        YamlInFormat -> projectYamlToProjectNative <$> readProjectYaml (toString inPath)
    case outFormat of
      StackOutFormat -> return ()
