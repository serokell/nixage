module Types where

-- |  Nixage cli command types

import Universum

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

