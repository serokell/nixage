module Nixage.Convert.Nix where

import Universum

import Data.Aeson (toJSON, fromJSON, Result(..))
import Nix.Convert
import Nix.Expr.Types (NExpr)
import Nix.Pretty (prettyNix, simpleExpr, NixDoc)

import Text.PrettyPrint.ANSI.Leijen
import Nixage.Project.Native (ProjectNative)

projectNativeToNix :: ProjectNative -> NExpr
projectNativeToNix = runIdentity . toNix

projectNativeToPrettyNix :: ProjectNative -> Doc
projectNativeToPrettyNix = prettyNix . projectNativeToNix
