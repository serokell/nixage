module Types where

-- |  Nixage cli command types

import Universum

data NixageCmd =
      StackCmd StackArgs
    | ConvertCmd ConvertArgs
      deriving (Show)

type StackArgs = [Text]

data ConvertArgs =
      YamlConvertIn Text ConvertOut
    | XConvertIn Text ConvertOut
    | ImplicitConvertIn ConvertOut
      deriving (Show)

data ConvertOut =
      StackConvertOut Text Text
    | YConvertOut Text
      deriving (Show)
