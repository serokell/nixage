import Universum

import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc)
import System.FilePath.Posix ((</>))
import System.IO.Temp (withSystemTempFile, withTempDirectory, withTempFile)
import System.Process (createProcess, delegate_ctlc, proc, waitForProcess)

import Nixage.Convert.FromYaml (decodeFromYaml)
import Nixage.Convert.Nix (projectNativeToPrettyNix)
import Nixage.Convert.Stack (encodeToStack)
import Paths_nixage (getDataFileName)

import Parser
import Types


main :: IO ()
main = execParser (info (helper <*> nixageP) infoMod) >>= \case
    StackCmd stackArgs     -> stackAction stackArgs
    ConvertCmd convertArgs -> convertAction convertArgs
  where
    infoMod = header "Nixage"
           <> progDesc "Build Haskell packages with Nix and Stackage"
           <> fullDesc


-- | * Nixage command actions

-- | Create temporary '[standard-tmp-dir]/nixage-snapshot[rand-num].yaml'
-- and './nixage-stack[rand-num].yaml', and run 'stack' on them.
stackAction :: (MonadIO m, MonadThrow m, MonadMask m) => StackArgs-> m ()
stackAction args = do
    projectNative <- decodeFromYaml "project.yaml"
    withSystemTempFile "nixage-stack-snapshot.yaml" $ \snapshotPath _ ->
      withTempFile "." "nixage-stack.yaml" $ \stackPath _ -> do
        withTempDirectory "." "nixage-stack-shell-dir" $ \stackShellDir -> do
          stackShellSourcePath <- liftIO $ getDataFileName "data/stack-shell.nix"
          let stackShellPath = stackShellDir </> "stack-shell.nix"
          encodeToStack stackPath snapshotPath stackShellPath stackShellSourcePath projectNative
          let  args' = ["--stack-yaml", toText stackPath] <> args
          liftIO $ do
              (_,_,_,handle) <- createProcess $
                   (proc "stack" (toString <$> args')) { delegate_ctlc = True }
              void $ waitForProcess handle

-- | Conversion between project specification formats.
convertAction :: (MonadIO m, MonadThrow m) => ConvertArgs -> m ()
convertAction (ConvertArgs convertIn convertOut) = do
    projectNative <- case convertIn of
      YamlConvertIn yamlPath -> decodeFromYaml (toString yamlPath)
    case convertOut of
      StackConvertOut stackPath snapshotPath stackShellPath -> do
        stackShellSourcePath <- liftIO $ getDataFileName "data/stack-shell.nix"
        encodeToStack (toString stackPath) (toString snapshotPath)
                      (toString stackShellPath) (toString stackShellSourcePath) projectNative
      NixConvertOut -> do
          print $ projectNativeToPrettyNix projectNative
