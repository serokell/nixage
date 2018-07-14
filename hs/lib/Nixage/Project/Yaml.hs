module Nixage.Project.Yaml
  ( ProjectYaml
  , pattern ProjectYaml
  , pResolver
  , pNixpkgs
  , pStackage
  , pExtraDeps

  , pattern HackageDepVersionYaml
  , pattern SourceDepVersionYaml
  ) where

import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import Data.Map (Map)
import Data.Text (Text)
import Data.Void (Void)
import Universum

import Nixage.Project.Extensible
import Nixage.Project.Types ( NixHash, NixpkgsVersion, StackageVersion
                            , PackageName, PackageVersion, ExternalSource(..)
                            , PackagePath)

-- | Nixage native AST marker
data AstYaml

-- | Nixage native project AST
type ProjectYaml = Project AstYaml

type instance XProject AstYaml = ()

type instance XHackageDepVersion AstYaml = ()
type instance XSourceDepVersion AstYaml = ()
type instance XXExtraDepVersion AstYaml = Void

deriving instance Show (Project AstYaml)
deriving instance Show (ExtraDepVersion AstYaml)

pattern ProjectYaml :: Text
                      -> (Maybe NixpkgsVersion)
                      -> (Maybe StackageVersion)
                      -> Map PackageName PackagePath
                      -> Map PackageName (ExtraDepVersion AstYaml)
                      -> Project AstYaml
pattern ProjectYaml r mnv msv mpv mpn = Project () r mnv msv mpv mpn

pattern HackageDepVersionYaml :: PackageVersion
                                -> ExtraDepVersion AstYaml
pattern HackageDepVersionYaml pv = HackageDepVersion () pv

pattern SourceDepVersionYaml :: ExternalSource
                               -> NixHash
                               -> Maybe FilePath
                               -> ExtraDepVersion AstYaml
pattern SourceDepVersionYaml es nh msd = SourceDepVersion () es nh msd

instance FromJSON (ExtraDepVersion AstYaml) where
    parseJSON (String s) = pure $ HackageDepVersionYaml s
    parseJSON (Object v) =
        SourceDepVersionYaml
        <$> externalSourceP
        <*> v .: "sha256"
        <*> v .: "subdir"
      where
        externalSourceP =
            GitSource
            <$> v .: "git"
            <*> v .: "rev"

instance FromJSON ProjectYaml where
    parseJSON (Object v) =
        ProjectYaml
        <$> v .: "resolver"
        <*> v .:? "nixpkgs"
        <*> v .:? "stackage"
        <*> v .: "packages"
        <*> (fromMaybe mempty <$> v .:? "extra-deps")
