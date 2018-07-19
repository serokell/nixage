module Nixage.Convert.Stack where

import Data.Map.Strict (mapWithKey, fromList, mapMaybe)
import Data.Aeson (ToJSON(..), object, (.=), Value(..))
import Universum

import Nixage.Project.Types ( PackageName, PackageVersion, PackagePath
                            , ExternalSource(GitSource), NixageError(..))
import Nixage.Project.Extensible (ExtraDepVersion)
import Nixage.Project.Native ( ProjectNative, pattern ProjectNative
                             , pattern HackageDepVersionNative
                             , pattern SourceDepVersionNative
                             , AstNixage)


data StackExtraDepVersion =
      StackHackageDepVersion PackageVersion
    | StackGitDepVersion Text Text (Maybe FilePath) -- git, rev, subdir

data StackCustomSnapshot = StackCustomSnapshot
    { scsName :: Text
    , scsResolver :: Text
    , scsPackages :: Map PackageName StackExtraDepVersion
    }

data StackConfig = StackConfig
    { scStackCustomSnapshot :: StackCustomSnapshot
    , scPackages :: Map PackageName PackagePath
    }

instance ToJSON StackCustomSnapshot where
    toJSON (StackCustomSnapshot name resolver packages) =
        object [ "name" .= name
               , "resolver" .= resolver , "packages" .= elems (mapWithKey packageToJSON packages) ]
      where
        packageToJSON :: PackageName -> StackExtraDepVersion -> Value
        packageToJSON name stackExtraDepVersion = case stackExtraDepVersion of
            StackHackageDepVersion packageVersion ->
                String $ name <> "-" <> packageVersion
            StackGitDepVersion git rev subdir ->
                let subdirs = maybeToList
                            $ (\subdir -> "subdirs" .= [subdir]) <$> subdir
                in object $ [ "git" .= git
                            , "commit" .= rev
                            ] <> subdirs

writeStackConfig :: StackConfig -> FilePath -> (Value, Value)
writeStackConfig (StackConfig snapshot packages) snapshotPath =
    (toJSON snapshot, stackYamlBuilder)
  where
    stackYamlBuilder = object
        [ "resolver" .= snapshotPath
        , "packages" .= elems packages
        , "nix" .= nix]

    nix = object
        [ "enable" .= True
        , ("shell-file", String "stack-shell.nix")]

-- | Convert ProjectNative AST to StackConfig
projectNativeToStackConfig :: (MonadThrow m)
                           => ProjectNative
                           -> m StackConfig
projectNativeToStackConfig (ProjectNative resolver _ _ mpp mpv) = do
    packages <- mapM toStackExtraDep mpv
    let snapshot = StackCustomSnapshot "nixage-stack-snapshot" resolver packages
    return $ StackConfig snapshot mpp
  where
    toStackExtraDep :: (MonadThrow m)
                    => ExtraDepVersion AstNixage
                    -> m StackExtraDepVersion
    toStackExtraDep (HackageDepVersionNative v) = return $ StackHackageDepVersion v
    toStackExtraDep (SourceDepVersionNative (GitSource git rev) _ msd) =
        return $ StackGitDepVersion  git rev msd
    toStackExtraDep _ = throwM $ ProjectNativeToStackConfigError "Extra dep source incompatible with Stack"

