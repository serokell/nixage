import Universum

import Data.Yaml (decodeFileEither, encodeFile)
import Options.Applicative ( info, fullDesc, progDesc, header
                           , execParser, helper)
import System.IO.Temp (withSystemTempFile, withTempFile)
import System.Process (waitForProcess, createProcess, delegate_ctlc, proc)

import Nixage.Project.Yaml (ProjectYaml, projectYamlToProjectNative)
import Nixage.Project.Types (NixageError(..))
import Nixage.Convert.Stack (createStackFiles, projectNativeToStackConfig, StackConfig)

import Types
import Parser

main :: IO ()
main = execParser (info nixageP infoMod) >>= \case
    StackCmd args -> stackAction args
    ConvertCmd convertArgs -> convertAction convertArgs
  where
    infoMod = header "Nixage"
           <> progDesc "Build Haskell packages with Nix and Stackage"
           <> fullDesc


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
