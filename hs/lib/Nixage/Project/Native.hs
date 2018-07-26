module Nixage.Project.Native
  ( ProjectNative
  , pattern ProjectNative
  , pResolver
  , pNixpkgs
  , pStackage
  , pExtraDeps
  , AstNixage

  , pattern HackageDepVersionNative
  , pattern SourceDepVersionNative
  ) where

import Universum hiding (toList)

import Data.Map (Map, toList)
import Data.Text (Text)
import Data.Void (Void)
import Nix.Convert (ToNix(..))
import Nix.Expr.Shorthands (mkNonRecSet, mkStr, ($=), attrsE)
import Nix.Expr.Types (NExpr)


import Nixage.Project.Extensible
import Nixage.Project.Types ( NixHash, NixpkgsVersion(..), StackageVersion(..)
                            , PackageName, PackageVersion, ExternalSource(..))


-- | Nixage native AST marker
data AstNixage

-- | Nixage native project AST
type ProjectNative = Project AstNixage

type instance XProject AstNixage = ()

type instance XHackageDepVersion AstNixage = ()
type instance XSourceDepVersion AstNixage = ()
type instance XXExtraDepVersion AstNixage = Void

deriving instance Show (Project AstNixage)
deriving instance Show (ExtraDepVersion AstNixage)


pattern ProjectNative :: Text
                      -> (Maybe NixpkgsVersion)
                      -> (Maybe StackageVersion)
                      -> Map PackageName FilePath
                      -> Map PackageName (ExtraDepVersion AstNixage)
                      -> Project AstNixage
pattern ProjectNative r mnv msv ps eds = Project () r mnv msv ps eds


pattern HackageDepVersionNative :: PackageVersion
                                -> ExtraDepVersion AstNixage
pattern HackageDepVersionNative pv = HackageDepVersion () pv

pattern SourceDepVersionNative :: ExternalSource
                               -> NixHash
                               -> Maybe FilePath
                               -> ExtraDepVersion AstNixage
pattern SourceDepVersionNative es nh msd = SourceDepVersion () es nh msd


instance ToNix (ExtraDepVersion AstNixage) Identity NExpr where
    toNix (HackageDepVersionNative s) = Identity $ mkStr s
    toNix (SourceDepVersionNative (GitSource git rev) sha256 subdir) =
        Identity $ mkNonRecSet $
            [ "git" $= mkStr git
            , "rev" $= mkStr rev
            , "sha256" $= mkStr sha256
            ]
            <> maybeToList (("subdir" $=) . mkStr . toText <$> subdir)

instance ToNix StackageVersion Identity NExpr where
    toNix (StackageVersion url sha256) = Identity $ mkNonRecSet
        [ "url" $= mkStr url
        , "sha256" $= mkStr sha256
        ]

instance ToNix NixpkgsVersion Identity NExpr where
    toNix (NixpkgsVersion url sha256) = Identity $ mkNonRecSet
        [ "url" $= mkStr url
        , "sha256" $= mkStr sha256
        ]

instance ToNix ProjectNative Identity NExpr where
    toNix (ProjectNative r mnv msv mpp mpv) = Identity $ mkNonRecSet $
        [ "resolver" $= mkStr r
        , "packages" $= packagesExpr
        ]
        <> maybeToList (("stackage" $=) . runIdentity . toNix <$> msv)
        <> maybeToList (("nixpkgs" $=) . runIdentity . toNix <$> mnv)
        <> (if null mpv then [] else [ "extra-deps" $= mpvExpr ])
      where
          packagesExpr = attrsE $ second (mkStr . toText)  <$> toList mpp
          mpvExpr = attrsE
                  $ second (runIdentity . toNix) <$> toList mpv
