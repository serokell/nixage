
import Universum

import Data.Yaml (decodeFileEither, encodeFile)
import Options.Applicative ( info, fullDesc, progDesc, header
                           , execParser, helper)
import System.IO.Temp (withSystemTempFile, withTempFile)
import System.Directory (copyFile)
import System.Process (waitForProcess, createProcess, delegate_ctlc, proc)

import Nixage.Project.Yaml (projectYamlToProjectNative, ProjectYaml)
import Nixage.Project.Types (NixageException(..))
import Nixage.Convert.Stack (createStackFiles, projectNativeToStackConfig, StackConfig)
import Nixage.Convert.Nix (projectNativeToPrettyNix)
import Paths_nixage (getDataFileName)

import Types
import Parser

import Types
import Parser

main :: IO ()
main = execParser (info (helper <*> nixageP) infoMod) >>= \case
    StackCmd stackArgs     -> stackAction stackArgs
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
      Left err -> throwM $ YamlDecodingException (show err)
      Right projectYaml -> return projectYaml

-- | Write stack and snapshot yaml files
writeStackConfig :: (MonadIO m, MonadThrow m)
                 => FilePath     -- ^ Stack yaml path
                 -> FilePath     -- ^ Snapshot yaml path
                 -> FilePath     -- ^ Stack shell  path
                 -> StackConfig
                 -> m ()
writeStackConfig stackPath snapshotPath stackShellPath stackConfig = do
   let (stack, snapshot) =
           createStackFiles stackConfig snapshotPath stackShellPath
   liftIO $ do
       encodeFile snapshotPath snapshot
       encodeFile stackPath stack

-- | Create temporary '[standard-tmp-dir]/nixage-snapshot[rand-num].yaml'
-- and './nixage-stack[rand-num].yaml', and run 'stack' on them.
stackAction :: (MonadIO m, MonadThrow m, MonadMask m) => StackArgs-> m ()
stackAction args = do
    liftIO (decodeFileEither "project.yaml") >>= \case
      Left err -> throwM $ YamlDecodingException (show err)
      Right projectYaml -> do
        let projectNative = projectYamlToProjectNative projectYaml
        let stackConfig = projectNativeToStackConfig projectNative
        withSystemTempFile "nixage-stack-snapshot.yaml" $ \snapshotPath _ ->
          withTempFile "." "nixage-stack.yaml" $ \stackPath _ -> do
            withTempFile "." "stack-shell.nix" $ \stackShellPath _ -> do
              stackShellSource <- liftIO $ getDataFileName "stack-shell.nix"
              liftIO $ copyFile stackShellSource stackShellPath
              writeStackConfig stackPath snapshotPath stackShellPath stackConfig
              let  args' = ["--stack-yaml", toText stackPath] <> args
              liftIO $ do
                  (_,_,_,handle) <- createProcess $
                       (proc "stack" (toString <$> args')) { delegate_ctlc = True }
                  void $ waitForProcess handle

-- | Conversion between project specification formats.
convertAction :: (MonadIO m, MonadThrow m) => ConvertArgs -> m ()
convertAction (ConvertArgs convertIn convertOut) = do
    projectNative <- case convertIn of
      YamlConvertIn yamlPath -> projectYamlToProjectNative <$> readProjectYaml (toString yamlPath)
    case convertOut of
      StackConvertOut stackPath snapshotPath stackShellPath -> do
        let stackConfig = projectNativeToStackConfig projectNative
        writeStackConfig (toString stackPath) (toString snapshotPath)
                         (toString stackShellPath) stackConfig
      NixConvertOut -> do
          print $ projectNativeToPrettyNix projectNative
