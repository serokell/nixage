module Nixage.Convert.Stack
       ( projectNativeToStackFiles
       , encodeToStack
       ) where

import Universum

import Control.Arrow ((>>>))
import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.HashMap.Strict as HM
import Data.Yaml (encode, encodeFile)

import Nixage.Project.Extensible (ExtraDepVersion (..), Project (..))
import Nixage.Project.Native (AstNixage, ProjectNative)
import Nixage.Project.Types (ExternalSource (GitSource), GhcOptions, PackageName, PackageVersion)


data StackExtraDepVersion =
      StackHackageDepVersion PackageVersion
    | StackGitDepVersion Text Text (Maybe FilePath) -- git, rev, subdir
    deriving Show

-- | Stack snapshot (name, resolver, packages)
data StackCustomSnapshot = StackCustomSnapshot
    Text                                        -- Snapshot name
    Text                                        -- Resolver
    (HashMap PackageName StackExtraDepVersion)  -- Packages (extra-deps)
    deriving Show

data StackConfig = StackConfig
    StackCustomSnapshot                -- Snapshot
    (HashMap PackageName FilePath)     -- Packages
    (Maybe GhcOptions)                 -- Ghc options
    deriving Show

instance ToJSON StackCustomSnapshot where
    toJSON (StackCustomSnapshot name resolver packages) =
        object [ "name" .= name
               , "resolver" .= resolver
               , "packages" .= map packageToJson (HM.toList packages)
               ]
      where
        packageToJson :: (PackageName, StackExtraDepVersion) -> Value
        packageToJson (packageName, StackHackageDepVersion packageVersion) =
            String $ packageName <> "-" <> packageVersion
        packageToJson (_, StackGitDepVersion url rev subdir) =
            object [ "git"     .= url
                   , "commit"  .= rev
                   , "subdirs" .= [fromMaybe "." subdir]
                   ]

-- | StackConfig json split into stack and snapshot jsons.
stackToJSON :: FilePath       -- ^ Snapshot path
            -> FilePath       -- ^ Stack shell path
            -> FilePath       -- ^ Stack shell source
            -> StackConfig
            -> (Value, Value, Text)
stackToJSON snapshotPath shellPath shellSourcePath (StackConfig stackCustomSnapshot packages mgo) =
    (stack, toJSON stackCustomSnapshot, stackShell)
  where
    stack = object
        [ "resolver" .= snapshotPath
        , "packages" .= elems packages
        , "nix" .= nix
        , "ghc-options" .= mgo
        ]

    nix = object
        [ "enable" .= True
        , "shell-file" .= shellPath
        ]

    stackShell = toText $ "import " <> shellSourcePath <>  " ./.."

-- | Convert ProjectNative AST to StackConfig
projectNativeToStackConfig :: ProjectNative -> StackConfig
projectNativeToStackConfig (Project () resolver _ _ ps eds go) =
    let packages = map toStackExtraDep eds
        snapshot = StackCustomSnapshot "nixage-stack-snapshot" resolver packages
    in StackConfig snapshot ps go
  where
    toStackExtraDep :: ExtraDepVersion AstNixage -> StackExtraDepVersion
    toStackExtraDep (HackageDepVersion () v) =
        StackHackageDepVersion v
    toStackExtraDep (SourceDepVersion () (GitSource git rev) _ msd) =
        StackGitDepVersion git rev msd
    toStackExtraDep (XExtraDepVersion v) =
        absurd v

-- | Pure native to stack conversion
projectNativeToStackFiles :: FilePath       -- ^ Snapshot path
                          -> FilePath       -- ^ Stack shell path
                          -> FilePath       -- ^ Stack shell source
                          -> ProjectNative
                          -> (ByteString, ByteString, ByteString)
projectNativeToStackFiles snapshotPath stackShellPath stackShellSourcePath =
        projectNativeToStackConfig
    >>> stackToJSON snapshotPath stackShellPath stackShellSourcePath
    >>> (\(x,y,z) -> (encode x, encode y, encode z))


-- | Conversion + IO that writes stack and snapshot yaml files
encodeToStack :: (MonadIO m, MonadThrow m)
                 => FilePath     -- ^ Stack yaml path
                 -> FilePath     -- ^ Snapshot yaml path
                 -> FilePath     -- ^ Stack shell  path
                 -> FilePath     -- ^ Stack shell source
                 -> ProjectNative
                 -> m ()
encodeToStack stackPath snapshotPath stackShellPath stackShellSourcePath =
        projectNativeToStackConfig
    >>> stackToJSON snapshotPath stackShellPath stackShellSourcePath
    >>> \(stack, snapshot, shell) -> liftIO $ do
            encodeFile snapshotPath snapshot
            encodeFile stackPath stack
            writeFile stackShellPath shell
